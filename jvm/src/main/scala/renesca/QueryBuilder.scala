package renesca

import renesca.graph._
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.table.Table

import scala.collection.mutable

case class QueryConfig(item: SubGraph, query: Query, callback: (Graph, Table) => Either[String, () => Any] = (graph: Graph, table: Table) => Right(() => ()))

class QueryPatterns(resolvedItems: mutable.Map[Item,Origin]) {
  private var variableCounter = 0
  def randomVariable = {
    val variable = "V" + variableCounter
    variableCounter += 1
    variable
  }

  def selectLiteralMap(variable: String, properties: Properties, selection: Set[PropertyKey]) = {
    val remainingProperties = properties.filterKeys(!selection.contains(_))
    val selectedProperties = properties.filterKeys(selection.contains(_))
    val parameterMap = selectedProperties.toMap.map { case (k, v) => (PropertyKey(s"${ variable }_${ k }"), v) }
    val literalMap = selectedProperties.map { case (k, _) => s"$k: {${ variable }_${ k }}" }
    val literalMapMatcher = if(literalMap.isEmpty) "" else literalMap.mkString("{", ",", "}")
    (literalMapMatcher, remainingProperties, parameterMap)
  }

  //TODO should limit number of matches for merge and match to 1!
  // match/merge ... with variable limit 1 ...
  def nodePattern(node: Node, forDeletion: Boolean = false): (String, String, String, ParameterMap, String) = {
    val variable = randomVariable
    val labels = node.labels.map(label => s":`$label`").mkString
    resolvedItems.getOrElse(node, node.origin) match {
      case Id(id)                =>
        ("match", s"($variable)", s"where id($variable) = {${ variable }_nodeId}", Map(s"${ variable }_nodeId" -> id), variable)
      case Create()              =>
        ("create", s"($variable $labels {${ variable }_properties})", "", Map(s"${ variable }_properties" -> node.properties.toMap), variable)
      case Merge(merge, onMatch) =>
        val (mergeLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, node.properties, merge.toSet)
        val onMatchProperties = remainingProperties.filterKeys(onMatch contains _)
        ("merge", s"($variable $labels $mergeLiteralMap)", s"on create set $variable += {${ variable }_onCreateProperties} on match set $variable += {${ variable }_onMatchProperties}",
          parameterMap ++ Map(s"${ variable }_onCreateProperties" -> remainingProperties.toMap, s"${ variable }_onMatchProperties" -> onMatchProperties.toMap),
          variable)
      case Match(matches)        =>
        val (matchLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, node.properties, matches)
        val (propertySetter, propertyMap) = if (forDeletion || remainingProperties.isEmpty)
          ("", Map.empty)
        else
          (s"set $variable += {${ variable }_properties}", Map(s"${ variable }_properties" -> remainingProperties.toMap))

        ("match", s"($variable $labels $matchLiteralMap)", propertySetter,
          parameterMap ++ propertyMap,
          variable)
    }
  }

  def relationPattern(relation: Relation, forDeletion: Boolean = false): (String, String, String, ParameterMap, String) = {
    val variable = randomVariable
    resolvedItems.getOrElse(relation, relation.origin) match {
      case Id(id)                =>
        ("match", s"[$variable]", s"where id($variable) = {${ variable }_relationId}", Map(s"${ variable }_relationId" -> id), variable)
      case Create()              =>
        ("create", s"[$variable :`${ relation.relationType }` {${ variable }_properties}]", "", Map(s"${ variable }_properties" -> relation.properties.toMap), variable)
      case Merge(merge, onMatch) =>
        val (mergeLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, relation.properties, merge)
        val onMatchProperties = remainingProperties.filterKeys(onMatch)
        ("merge", s"[$variable :`${ relation.relationType }` $mergeLiteralMap]", s"on create set $variable += {${ variable }_onCreateProperties} on match set $variable += {${ variable }_onMatchProperties}",
          parameterMap ++ Map(s"${ variable }_onCreateProperties" -> remainingProperties.toMap, s"${ variable }_onMatchProperties" -> onMatchProperties.toMap),
          variable)
      case Match(matches)        =>
        val (matchLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(variable, relation.properties, matches)
        val (propertySetter, propertyMap) = if (forDeletion || remainingProperties.isEmpty)
          ("", Map.empty)
        else
          (s"set $variable += {${ variable }_properties}", Map(s"${ variable }_properties" -> remainingProperties.toMap))

        ("match", s"[$variable :`${ relation.relationType }` $matchLiteralMap]", propertySetter,
          parameterMap ++ propertyMap,
          variable)
    }
  }

  def queryNode(node: Node) = {
    val (keyword, query, postfix, parameters, variable) = nodePattern(node)
    Query(s"$keyword $query $postfix return $variable", parameters)
  }

  def queryRelationPattern(relation: Relation, forDeletion: Boolean = false) = {
    if(resolvedItems.getOrElse(relation.startNode, relation.startNode.origin).isLocal)
      throw new Exception("Start node in relation is still local: " + relation)
    if(resolvedItems.getOrElse(relation.endNode, relation.endNode.origin).isLocal)
      throw new Exception("End node in relation is still local: " + relation)

    val (startKeyword, startQuery, startPostfix, startParameters, startVariable) = nodePattern(relation.startNode, forDeletion)
    val (endKeyword, endQuery, endPostfix, endParameters, endVariable) = nodePattern(relation.endNode, forDeletion)
    val (keyword, query, postfix, parameters, variable) = relationPattern(relation, forDeletion)

    (s"$startKeyword $startQuery $startPostfix $endKeyword $endQuery $endPostfix $keyword ($startVariable)-$query->($endVariable) $postfix", variable, startParameters ++ parameters ++ endParameters)
  }

  def queryRelation(relation: Relation) = {
    val (queryPattern, variable, params) = queryRelationPattern(relation)
    Query(s"$queryPattern return $variable", params)
  }

  def queryPathPattern(path: Path, forDeletion: Boolean = false) = {
    val variableMap = mutable.LinkedHashMap.empty[Item, String]

    // first match all non-path nodes
    val boundNodes = path.relations.flatMap(r => Seq(r.startNode, r.endNode)).distinct diff path.nodes
    if(boundNodes.exists(n => resolvedItems.getOrElse(n, n.origin).isLocal))
      throw new Exception("Bound node on path is still local: " + path)

    val (nodeQueries, nodeParameters) = boundNodes.map(node => {
      val (keyword, query, postfix, parameters, variable) = nodePattern(node, forDeletion)
      variableMap += node -> variable
      (s"$keyword $query $postfix", parameters.toMap)
    }).unzip

    val nodeQuery = nodeQueries.mkString(" ")

    // now the actual path can matched/merged/created
    val (pathQuery, pathParameters) = {
      val (pathQueries, pathSetters, pathParameters) = path.relations.zipWithIndex.map { case (relation, i) =>
        // a path assures that the start node was already matched by the previous relation (if there is one)
        val (_, startQuery, startSetter, startParameters, startVariable) = if(i == 0)
                                                                             variableMap.get(relation.startNode).map(variable => ("", s"($variable)", "", Map.empty, variable)).getOrElse(nodePattern(relation.startNode, forDeletion))
                                                                           else
                                                                             ("", "", "", Map.empty, variableMap(relation.startNode))
        val (_, endQuery, endSetter, endParameters, endVariable) = variableMap.get(relation.endNode).map(variable => ("", s"($variable)", "", Map.empty, variable)).getOrElse(nodePattern(relation.endNode, forDeletion))
        val (_, relQuery, relSetter, relParameters, relVariable) = relationPattern(relation, forDeletion)
        variableMap ++= Seq(relation.startNode -> startVariable, relation.endNode -> endVariable, relation -> relVariable)

        (s"$startQuery-$relQuery->$endQuery",
          s"$startSetter $endSetter $relSetter",
          startParameters ++ endParameters ++ relParameters)
      }.unzip3

      val parameters = pathParameters.flatten.toMap
      val pathQuery = s"""${ pathQueries.mkString(" ") } ${ pathSetters.mkString(" ") }"""
      (path.origin match {
        case Create()    => s"create $pathQuery"
        case Match(_)    => s"match $pathQuery"
        case Merge(_, _) => s"merge $pathQuery"
      }, parameters)
    }

    val parameters = (nodeParameters.flatten ++ pathParameters).toMap

    (s"$nodeQuery $pathQuery", parameters, variableMap)
  }

  def queryPath(path: Path) = {
    val (queryPattern, parameters, variableMap) = queryPathPattern(path)
    val reverseVariableMap = variableMap.map { case (k, v) => (v, k) }.toMap

    val returnClause = "return " + variableMap.map {
      case (k, variable) => k match {
        case _: Node     => s"{id: id($variable), properties: $variable, labels: labels($variable)} as $variable"
        case _: Relation => s"{id: id($variable), properties: $variable} as $variable"
      }
    }.mkString(",")

    (Query(s"$queryPattern $returnClause", parameters), reverseVariableMap)
  }
}

class QueryGenerator {
  val resolvedItems: mutable.Map[Item,Origin] = mutable.Map.empty

  def contentChangesToQueries(contentChanges: Seq[GraphContentChange])() = {
    contentChanges.groupBy(_.item).map {
      case (item, changes) =>
        if(item.origin.isLocal)
          throw new Exception("Trying to edit local item: " + item)

        val propertyAdditions: mutable.Map[PropertyKey, ParameterValue] = mutable.Map.empty
        val propertyRemovals: mutable.Set[PropertyKey] = mutable.Set.empty
        val labelAdditions: mutable.Set[Label] = mutable.Set.empty
        val labelRemovals: mutable.Set[Label] = mutable.Set.empty
        changes.foreach {
          case SetProperty(_, key, value) =>
            propertyRemovals -= key
            propertyAdditions += key -> value
          case RemoveProperty(_, key)     =>
            propertyRemovals += key
            propertyAdditions -= key
          case SetLabel(_, label)         =>
            labelRemovals -= label
            labelAdditions += label
          case RemoveLabel(_, label)      =>
            labelRemovals += label
            labelAdditions -= label
        }

        val isRelation = item match {
          case _: Node     => false
          case _: Relation => true
        }

        val qPatterns = new QueryPatterns(resolvedItems)
        val variable = qPatterns.randomVariable
        val matcher = if(isRelation) s"match ()-[$variable]->()" else s"match ($variable)"
        val propertyRemove = propertyRemovals.map(r => s"remove $variable.`$r`").mkString(" ")
        val labelAdd = labelAdditions.map(a => s"set $variable:`$a`").mkString(" ")
        val labelRemove = labelRemovals.map(r => s"remove $variable:`$r`").mkString(" ")
        val setters = s"set $variable += {${ variable }_propertyAdditions} $propertyRemove $labelAdd $labelRemove"
        val setterMap = Map(s"${ variable }_propertyAdditions" -> propertyAdditions.toMap)

        val query = Query(
          s"$matcher where id($variable) = {${ variable }_itemId} $setters",
          Map(s"${ variable }_itemId" -> item.origin.asInstanceOf[Id].id) ++ setterMap
        )

        QueryConfig(item, query)
    }.toSeq
  }

  def deletionToQueries(deleteItems: Seq[Item])() = {
    deleteItems.map { item =>
      val qPatterns = new QueryPatterns(resolvedItems)
      item match {
        case n: Node =>
          val (keyword, query, postfix, parameters, variable) = qPatterns.nodePattern(n, forDeletion = true)
          val optionalVariable = qPatterns.randomVariable
          QueryConfig(n, Query(s"$keyword $query $postfix optional match ($variable)-[$optionalVariable]-() delete $optionalVariable, $variable", parameters))
        case r: Relation =>
          //TODO: invalidate Id origin of deleted item?
          if (!r.origin.isLocal) { // if not local we can match by id and have a simpler query
            val (keyword, query, postfix, parameters, variable) = qPatterns.relationPattern(r,  forDeletion =true)
            QueryConfig(r, Query(s"$keyword ()-$query-() $postfix delete $variable", parameters))
          } else {
            val (queryPattern, variable, params) = qPatterns.queryRelationPattern(r, forDeletion = true)
            QueryConfig(r, Query(s"$queryPattern delete $variable", params))
          }
      }
    }
  }

  def deletionPathsToQueries(deletePaths: Seq[Path])() = {
    deletePaths.map { path =>
      val qPatterns = new QueryPatterns(resolvedItems)
      val (queryPattern, parameters, variableMap) = qPatterns.queryPathPattern(path, forDeletion = true)

      val (optionalMatchers, matcherVariables) = path.nodes.map { n =>
        val optionalVariable = qPatterns.randomVariable
        val variable = variableMap(n)
        (s"optional match ($variable)-[$optionalVariable]-()", optionalVariable)
      }.unzip

      val variables = (path.relations ++ path.nodes).map(variableMap)
      val returnClause = "delete " + (matcherVariables ++ variables).mkString(",")

      QueryConfig(path, Query(s"$queryPattern ${optionalMatchers.mkString(" ")} $returnClause", parameters))
    }
  }

  def addPathsToQueries(addPaths: Seq[Path])() = {
    addPaths.map(path => {
      val qPatterns = new QueryPatterns(resolvedItems)
      val (query, reverseVariableMap) = qPatterns.queryPath(path)

      QueryConfig(path, query, (graph: Graph, table: Table) => {
        if(table.rows.size > 1)
          Left("More than one query result for path: " + path)
        else
          table.rows.headOption.map(row => {
            table.columns.foreach(col => {
              val item = reverseVariableMap(col)
              val map = row(col).asInstanceOf[MapParameterValue].value
              resolvedItems += item -> Id(map("id").asInstanceOf[LongPropertyValue].value)
            })
            Right(() => {
              table.columns.foreach(col => {
                val item = reverseVariableMap(col)
                val map = row(col).asInstanceOf[MapParameterValue].value
                // TODO: without casts?
                item.origin = Id(map("id").asInstanceOf[LongPropertyValue].value)
                item.properties.clear()
                item.properties ++= map("properties").asInstanceOf[MapParameterValue].value.asInstanceOf[PropertyMap]
                item match {
                  case n: Node =>
                    n.labels.clear()
                    n.labels ++= map("labels").asInstanceOf[ArrayParameterValue].value.asInstanceOf[Seq[StringPropertyValue]].map(l => Label(l.value))
                  case _       =>
                }
              })
            })
          }).getOrElse(Left("Query result is missing desired path: " + path))
      })
    })
  }

  def addRelationsToQueries(addRelations: Seq[Relation])() = {
    addRelations.map(relation => {
      val qPatterns = new QueryPatterns(resolvedItems)
      val query = qPatterns.queryRelation(relation)

      QueryConfig(relation, query, (graph: Graph, table: Table) => {
        if(graph.relations.size > 1)
          Left("More than one query result for relation: " + relation)
        else
          graph.relations.headOption.map(dbRelation => {
            resolvedItems += relation -> dbRelation.origin
            Right(() => {
              relation.properties.clear()
              relation.properties ++= dbRelation.properties
              relation.origin = dbRelation.origin
            })
          }).getOrElse(Left("Query result is missing desired relation: " + relation))
      })
    })
  }

  def addNodesToQueries(addNodes: Seq[Node])() = {
    addNodes.map(node => {
      val qPatterns = new QueryPatterns(resolvedItems)
      val query = qPatterns.queryNode(node)

      QueryConfig(node, query, (graph: Graph, table: Table) => {
        if(graph.nodes.size > 1)
          Left("More than one query result for node: " + node)
        else
          graph.nodes.headOption.map(dbNode => {
            resolvedItems += node -> dbNode.origin
            Right(() => {
              node.properties.clear()
              node.labels.clear()
              node.properties ++= dbNode.properties
              node.labels ++= dbNode.labels
              node.origin = dbNode.origin
            })
          }).getOrElse(Left("Query result is missing desired node: " + node))
      })
    })
  }
}

class PathDependencyGraph(paths: Set[Path]) {
  private val resolved: mutable.ArrayBuffer[Path] = mutable.ArrayBuffer.empty
  private val seen: mutable.Set[Path] = mutable.Set.empty
  private val pathCreators: Map[Node, Path] = paths.flatMap(p => p.nodes.map(_ -> p).toMap).toMap

  case class CircularException(path: Path, dependency: Path) extends Exception

  private def resolvePath(path: Path): Unit = {
    if(resolved.contains(path))
      return

    seen += path
    val readNodes = path.allNodes.diff(path.nodes).filter(_.origin.isLocal)
    val dependencies = readNodes.flatMap(pathCreators.get(_))
    dependencies.foreach(dep => {
      if(!resolved.contains(dep)) {
        if(seen.contains(dep)) {
          throw CircularException(path, dep)
        }

        resolvePath(dep)
      }
    })

    resolved += path
  }

  def resolvePaths: Either[String, Seq[Seq[Path]]] = {
    try {
      paths.foreach(p => resolvePath(p))
    } catch {
      case CircularException(path, dep) =>
        return Left(s"Circular dependency between paths: $path depends on $dep")
    }

    Right(resolved.map(Seq(_)))
  }
}

class QueryBuilder {

  protected def newQueryGenerator = new QueryGenerator

  protected def newPathDependencyGraph(paths: Set[Path]) = new PathDependencyGraph(paths)

  protected def filterGraphChanges(graphChanges: Seq[GraphChange]) = {
    // First get rid of duplicate changes, keep only the last change
    val reversedDistinctChanges = graphChanges.reverse.distinct

    // handle deletions
    val deletedItems = mutable.Set.empty[Item]
    val addedItems = mutable.Set.empty[Item]
    reversedDistinctChanges.filter {
      case DeleteItem(item)   =>
        if(addedItems.contains(item))
          false
        else {
          deletedItems += item
          true
        }
      case AddItem(item)      =>
        if(deletedItems.contains(item))
          false
        else {
          addedItems += item
          true
        }
      case c: GraphItemChange =>
        !deletedItems.contains(c.item)
      case _                  =>
        true
    }.reverse
  }

  protected def checkChanges(allChanges: Seq[GraphChange], deleteItems: Seq[Item], deletePaths: Seq[Path], addPaths: Seq[Path], addLocalPaths: Seq[Path], addRelations: Seq[Relation]): Option[String] = {
    val illegalChange = allChanges.find(!_.isValid)
    if(illegalChange.isDefined)
      return Some("Found invalid graph change: " + illegalChange.get)

    // check whether paths try to resolve the same items
    val pathNodes = addPaths.flatMap(_.nodes)
    if(pathNodes.distinct.size != pathNodes.size) {
      return Some("Paths cannot resolve the same nodes")
    }

    val pathRelations = addPaths.flatMap(_.relations)
    if(pathRelations.distinct.size != pathRelations.size) {
      return Some("Paths cannot resolve the same relations")
    }

    // check whether nodes in a new relation should also be deleted
    if(deleteItems.intersect(addRelations.flatMap(r => Seq(r.startNode, r.endNode))).nonEmpty) {
      return Some("Cannot delete start- or endnode of a new relation: " + deleteItems.mkString(","))
    }

    // each path can only be deleted as a whole, there is no defined way to match partial paths.
    val incompletePath = deletePaths.find { path =>
      val pathItems = path.nodes ++ path.relations
      val intersection = deleteItems.intersect(pathItems)
      pathItems.length != intersection.length
    }

    if (incompletePath.isDefined)
      return Some("Cannot partially match paths, will not delete parts of a path: " + incompletePath.get)

    None
  }

  def generateQueries(graphChanges: Seq[GraphChange]): Either[String, Seq[() => Seq[QueryConfig]]] = {
    val gen = newQueryGenerator

    val changes = filterGraphChanges(graphChanges)

    val deleteItems = changes.collect { case DeleteItem(i) => i }

    // gather content changes
    val contentChanges = changes.collect { case c: GraphContentChange => c }

    // gather path changes, we ignore all paths that reference deleted items
    val (deletePaths, addPaths) = changes.collect { case AddPath(p) => p }.partition(p => deleteItems.intersect(p.allNodes ++ p.relations).nonEmpty)
    val (addLocalProducePaths, addLocalRelationPaths, addNonLocalPaths) = {
      val (addLocalPaths, addNonLocalPaths) = addPaths.partition(p => p.allNodes.diff(p.nodes).exists(_.origin.isLocal))
      val (addLocalProducePaths, addLocalRelationPaths) = addLocalPaths.partition(p => p.nodes.nonEmpty)

      (addLocalProducePaths, addLocalRelationPaths, addNonLocalPaths)
    }

    // all add-relation and add-node changes that are already included in paths need to be ignored,
    // as they are handled when adding the paths itself
    val addRelations = changes.collect { case AddItem(r: Relation) => r }.filterNot(addPaths.flatMap(_.relations).toSet)
    val (addLocalRelations, addNonLocalRelations) = addRelations.partition(r => r.startNode.origin.isLocal || r.endNode.origin.isLocal)
    val addNodes = changes.collect { case AddItem(n: Node) => n }.filterNot(addPaths.flatMap(_.nodes).toSet)

    // translate origin of delete items. merge nodes and relations can be
    // matched via their merge properties, so we can delete them like match
    // items.
    deleteItems.foreach( item => item.origin = item.origin match {
        case Merge(merge, _) => Match(merge)
        case other           => other
    })

    deletePaths.foreach( item => item.origin = item.origin match {
        case Merge(merge, _) => Match(merge)
        case other           => other
    })

    checkChanges(changes, deleteItems, deletePaths, addPaths, addLocalProducePaths, addRelations) match {
      case Some(err) => return Left(err)
      case None      =>
    }

    // gather delete changes
    val (independentDeleteItems, dependentDeleteRelations) = {
      val filteredDeleteItems = deleteItems.filterNot(deletePaths.flatMap(p => p.nodes ++ p.relations).toSet)
      val deleteNodes = filteredDeleteItems.collect { case n: Node => n }
      val filteredDeleteNodes = deleteNodes.filter(i => !i.origin.isLocal || i.origin.kind == Match.kind)
      val deleteRelations = filteredDeleteItems.collect { case r:Relation => r }.filter { relation =>
        // deleting a node deletes all connected relations, thus relation
        // deletion is already handled if the start- or endnode is deleted.
        // create relation can be ignored
        !deleteNodes.contains(relation.startNode) && !deleteNodes.contains(relation.endNode) && (!relation.origin.isLocal || relation.origin.kind == Match.kind)
      }

      // split delete changes into dependent and independent changes
      // independent changes that are directly handled in the first request:
      // 1. all nodes
      // 2. relations that can be referenced by their id (non-local relation)
      // 2. relations that can be referenced by the id of their non-local start- and endnode
      // all other delete changes are send in the last request, when everything is resolved
      val (deleteLocalRelations, deleteNonLocalRelations) = deleteRelations.partition(r => r.origin.isLocal && (r.startNode.origin.isLocal || r.endNode.origin.isLocal))

      (filteredDeleteNodes ++ deleteNonLocalRelations, deleteLocalRelations)
    }

    val filteredDeletePaths = deletePaths.filter(p => !p.origin.isLocal || p.origin.kind == Match.kind)

    // independent changes that do not dependent on the results of any other
    // change request and can be sent in one request
    val independentChanges = {
      val contentChangeQueries = gen.contentChangesToQueries(contentChanges) _
      val deleteQueries = gen.deletionToQueries(independentDeleteItems) _
      val addNodeQueries = gen.addNodesToQueries(addNodes) _
      val addNonLocalPathQueries = gen.addPathsToQueries(addNonLocalPaths) _
      val addNonLocalRelationQueries = gen.addRelationsToQueries(addNonLocalRelations) _

      () => contentChangeQueries() ++
        deleteQueries() ++
        addNodeQueries() ++
        addNonLocalPathQueries() ++
        addNonLocalRelationQueries()
    }

    // node producing paths that can be referenced by relations and might
    // reference nodes created in the first request the dependency graph
    // resolves dependencies between the paths, and sends a request for each
    // independent chunk
    val localProducePathChunks = newPathDependencyGraph(addLocalProducePaths.toSet).resolvePaths match {
      case Left(err)     => return Left(err)
      case Right(chunks) => chunks.map(c => gen.addPathsToQueries(c) _)
    }

    // after all possible node producing queries are resolved, the relations
    // can be handled in the last request
    val relationChanges = {
      val addLocalRelationQueries = gen.addRelationsToQueries(addLocalRelations) _
      val addLocalRelationPathQueries = gen.addPathsToQueries(addLocalRelationPaths) _
      val deleteQueries = gen.deletionToQueries(dependentDeleteRelations) _
      val deletePathQueries = gen.deletionPathsToQueries(filteredDeletePaths) _

      () => addLocalRelationQueries() ++
        addLocalRelationPathQueries() ++
        deleteQueries() ++
        deletePathQueries()
    }

    Right(
      Seq(independentChanges) ++
        localProducePathChunks ++
        Seq(relationChanges)
    )
  }

  def applyQueries(queryRequests: Seq[() => Seq[QueryConfig]], queryHandler: (Seq[Query]) => Seq[(Graph, Table)]): Option[String] = {
    val handles = queryRequests.view.flatMap(getter => {
      val configs = getter()
      if (configs.isEmpty)
        Seq.empty
      else {
        val (queries, callbacks) = configs.map(c => (c.query, c.callback)).unzip
        queryHandler(queries).zip(callbacks).view.map { case ((g, t), f) => f(g, t) }
      }
    })

    var failure: Option[String] = None
    val successHandles = handles.takeWhile(h => {
      failure = h.left.toOption
      failure.isEmpty
    }).force

    if(failure.isEmpty)
      successHandles.foreach(_.right.get())

    failure
  }
}
