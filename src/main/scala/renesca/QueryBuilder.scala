package renesca

import renesca.graph._
import renesca.helpers._
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.table.Table

import scala.collection.mutable

case class QueryConfig(item: SubGraph, query: Query, callback: (Graph, Table) => Boolean = (graph: Graph, table: Table) => true)

class QueryBuilder {

  def randomVariable = "V" + java.util.UUID.randomUUID().toString.replace("-", "")

  private def selectLiteralMap(variable: String, properties: Properties, selection: Set[PropertyKey]) = {
    val remainingProperties = properties.filterKeys(!selection.contains(_))
    val selectedProperties = properties.filterKeys(selection.contains(_))
    val parameterMap = selectedProperties.toMap.map { case (k, v) => (PropertyKey(s"${ variable }_${ k }"), v) }
    val literalMap = selectedProperties.map { case (k, _) => s"$k: {${ variable }_${ k }}" }
    val literalMapMatcher = if(literalMap.isEmpty) "" else literalMap.mkString("{", ",", "}")
    (literalMapMatcher, remainingProperties, parameterMap)
  }

  //TODO should limit number of matches for merge and match to 1!
  // match/merge ... with variable limit 1 ...
  private def nodePattern(node: Node): (String, String, String, ParameterMap, String) = {
    val variable = randomVariable
    val labels = node.labels.map(label => s":`$label`").mkString
    node.origin match {
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
        ("match", s"($variable $labels $matchLiteralMap)", s"set $variable += {${ variable }_properties}",
          parameterMap ++ Map(s"${ variable }_properties" -> remainingProperties.toMap),
          variable)
    }
  }

  private def relationPattern(relation: Relation): (String, String, String, ParameterMap, String) = {
    val variable = randomVariable
    relation.origin match {
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
        ("match", s"[$variable :`${ relation.relationType }` $matchLiteralMap]", s"set $variable += {${ variable }_properties}",
          parameterMap ++ Map(s"${ variable }_properties" -> remainingProperties.toMap),
          variable)
    }
  }

  private def queryNode(node: Node) = {
    val (keyword, query, postfix, parameters, variable) = nodePattern(node)
    Query(s"$keyword $query $postfix return $variable", parameters)
  }

  private def queryRelation(relation: Relation) = {
    if(relation.startNode.origin.isLocal)
      throw new Exception("Start node in relation is still local: " + relation)
    if(relation.endNode.origin.isLocal)
      throw new Exception("End node in relation is still local: " + relation)

    val (startKeyword, startQuery, startPostfix, startParameters, startVariable) = nodePattern(relation.startNode)
    val (endKeyword, endQuery, endPostfix, endParameters, endVariable) = nodePattern(relation.endNode)
    val (keyword, query, postfix, parameters, variable) = relationPattern(relation)
    Query(s"$startKeyword $startQuery $startPostfix $endKeyword $endQuery $endPostfix $keyword ($startVariable)-$query->($endVariable) $postfix return $variable",
      startParameters ++ parameters ++ endParameters)
  }

  private def queryPath(path: Path) = {
    val variableMap = mutable.LinkedHashMap.empty[Item, String]

    // first match all non-path nodes
    val boundNodes = path.relations.flatMap(r => Seq(r.startNode, r.endNode)).distinct diff path.nodes
    if(boundNodes.exists(_.origin.isLocal))
      throw new Exception("Bound node on path is still local: " + path)

    val (nodeQueries, nodeParameters) = boundNodes.map(node => {
      val (keyword, query, postfix, parameters, variable) = nodePattern(node)
      variableMap += node -> variable
      (s"$keyword $query $postfix", parameters.toMap)
    }).unzip

    val nodeQuery = nodeQueries.mkString(" ")

    // now the actual path can matched/merged/created
    val (pathQuery, pathParameters) = {
      val (pathQueries, pathSetters, pathParameters) = path.relations.zipWithIndex.map { case (relation, i) =>
        // a path assures that the start node was already matched by the previous relation (if there is one)
        val (_, startQuery, startSetter, startParameters, startVariable) = if(i == 0)
                                                                             variableMap.get(relation.startNode).map(variable => ("", s"($variable)", "", Map.empty, variable)).getOrElse(nodePattern(relation.startNode))
                                                                           else
                                                                             ("", "", "", Map.empty, variableMap(relation.startNode))
        val (_, endQuery, endSetter, endParameters, endVariable) = variableMap.get(relation.endNode).map(variable => ("", s"($variable)", "", Map.empty, variable)).getOrElse(nodePattern(relation.endNode))
        val (_, relQuery, relSetter, relParameters, relVariable) = relationPattern(relation)
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

    val reverseVariableMap = variableMap.map { case (k, v) => (v, k) }.toMap
    val parameters = (nodeParameters.flatten ++ pathParameters).toMap

    val returnClause = "return " + variableMap.map {
      case (k, variable) => k match {
        case _: Node     => s"{id: id($variable), properties: $variable, labels: labels($variable)} as $variable"
        case _: Relation => s"{id: id($variable), properties: $variable} as $variable"
      }
    }.mkString(",")

    (Query(s"$nodeQuery $pathQuery $returnClause", parameters), reverseVariableMap)
  }

  private def contentChangesToQueries(contentChanges: Seq[GraphContentChange])() = {
    contentChanges.groupBy(_.item).map {
      case (item, changes) =>
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

        val variable = randomVariable
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

  private def deletionToQueries(deleteItems: Seq[Item])() = {
    deleteItems.map {
      case n: Node     =>
        if(n.origin.isLocal)
          throw new Exception("Trying to delete local node: " + n)

        val (keyword, query, postfix, parameters, variable) = nodePattern(n)
        val optionalVariable = randomVariable
        QueryConfig(n, Query(s"$keyword $query $postfix optional match ($variable)-[$optionalVariable]-() delete $optionalVariable, $variable", parameters))
      case r: Relation =>
        if(r.origin.isLocal)
          throw new Exception("Trying to delete local relation: " + r)

        val (keyword, query, postfix, parameters, variable) = relationPattern(r)
        //TODO: invalidate Id origin of deleted item?
        QueryConfig(r, Query(s"$keyword ()-$query-() $postfix delete $variable", parameters))
    }
  }

  private def addPathsToQueries(addPaths: Seq[Path])() = {
    addPaths.map(path => {
      val (query, reverseVariableMap) = queryPath(path)

      QueryConfig(path, query, (graph: Graph, table: Table) => {
        table.columns.forall(col => {
          val item = reverseVariableMap(col)
          table.rows.headOption.exists(row => {
            val map = row(col).asInstanceOf[MapParameterValue].value
            item match {
              case n: Node     =>
                n.properties.clear()
                n.labels.clear()
                // TODO: without casts?
                n.properties ++= map("properties").asInstanceOf[MapParameterValue].value.asInstanceOf[PropertyMap]
                n.labels ++= map("labels").asInstanceOf[ArrayParameterValue].value.asInstanceOf[Seq[StringPropertyValue]].map(l => Label(l.value))
                n.origin = Id(map("id").asInstanceOf[LongPropertyValue].value)
              case r: Relation =>
                r.properties.clear()
                r.properties ++= map("properties").asInstanceOf[MapParameterValue].value.asInstanceOf[PropertyMap]
                r.origin = Id(map("id").asInstanceOf[LongPropertyValue].value)
            }

            true
          })
        })
      })
    })
  }

  private def addRelationsToQueries(addRelations: Seq[Relation])() = {
    addRelations.map(relation => {
      val query = queryRelation(relation)

      QueryConfig(relation, query, (graph: Graph, table: Table) => {
        graph.relations.headOption.exists(dbRelation => {
          relation.properties.clear()
          relation.properties ++= dbRelation.properties
          relation.origin = dbRelation.origin
          true
        })
      })
    })
  }

  private def addNodesToQueries(addNodes: Seq[Node])() = {
    addNodes.map(node => {
      val query = queryNode(node)

      QueryConfig(node, query, (graph: Graph, table: Table) => {
        graph.nodes.headOption.exists(dbNode => {
          node.properties.clear()
          node.labels.clear()
          node.properties ++= dbNode.properties
          node.labels ++= dbNode.labels
          node.origin = dbNode.origin
          true
        })
      })
    })
  }

  //TODO: test separately
  def filterGraphChanges(graphChanges: Seq[GraphChange]) = {
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

  private def checkChanges(deleteItems: Seq[Item], addPaths: Seq[Path], addLocalPaths: Seq[Path], addRelations: Seq[Relation]): Option[String] = {
    // TODO: more efficient!
    // check whether paths overlap
    val pathNodes = addPaths.flatMap(_.nodes)
    if(pathNodes.distinct.size != pathNodes.size) {
      return Some("Paths cannot resolve the same nodes")
    }

    val pathRelations = addPaths.flatMap(_.relations)
    if(pathRelations.distinct.size != pathRelations.size) {
      return Some("Paths cannot resolve the same relations")
    }

    // check whether local paths depend on each other
    // currenlty paths cannot contain nodes which are created by another path
    val dependentPath = addLocalPaths.find(p => p.allNodes.diff(p.nodes).intersect(pathNodes).nonEmpty)
    val dependentPathNodes = addLocalPaths.flatMap(p => p.allNodes.filter(_.origin.isLocal))
    if(dependentPath.isDefined) {
      return Some("Overlapping paths with local nodes are currently not supported")
    }

    // check whether nodes in a new relation or items in a new path should also be deleted, this is not possible!
    if(deleteItems.intersect(addPaths.flatMap(p => p.relations ++ p.allNodes)).nonEmpty) {
      return Some("Cannot delete item which is contained in a path: " + deleteItems.mkString(","))
    }
    if(deleteItems.intersect(addRelations.flatMap(r => Seq(r.startNode, r.endNode))).nonEmpty) {
      return Some("Cannot delete start- or endnode of a new relation: " + deleteItems.mkString(","))
    }

    None
  }

  def generateQueries(graphChanges: Seq[GraphChange]): Either[String, Seq[() => Seq[QueryConfig]]] = {
    // filter graph changes
    val changes = filterGraphChanges(graphChanges)

    // gather deletion changes
    val deleteItems = changes.collect { case DeleteItem(i) => i }
    val nonLocalDeleteItems = deleteItems.filterNot(_.origin.isLocal)

    // gather content changes
    val contentChanges = changes.collect { case c: GraphContentChange => c }

    // gather path changes
    val addPaths = changes.collect { case AddPath(p) => p }
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

    checkChanges(deleteItems, addPaths, addLocalProducePaths, addRelations) match {
      case Some(err) => return Left(err)
      case None           =>
    }

    // generate queries
    val independentChanges = {
      val contentChangeQueries = contentChangesToQueries(contentChanges) _
      val deleteQueries = deletionToQueries(nonLocalDeleteItems) _
      val addNodeQueries = addNodesToQueries(addNodes) _
      val addNonLocalPathQueries = addPathsToQueries(addNonLocalPaths) _
      val addNonLocalRelationQueries = addRelationsToQueries(addNonLocalRelations) _

      () => contentChangeQueries() ++
        deleteQueries() ++
        addNodeQueries() ++
        addNonLocalPathQueries() ++
        addNonLocalRelationQueries()
    }

    val producePathChanges = addPathsToQueries(addLocalProducePaths) _

    val relationChanges = {
      val addLocalRelationQueries = addRelationsToQueries(addLocalRelations) _
      val addLocalRelationPathQueries = addPathsToQueries(addLocalRelationPaths) _

      () => addLocalRelationQueries() ++
        addLocalRelationPathQueries()
    }

    Right(Seq(
      independentChanges,
      producePathChanges,
      relationChanges
    ))
  }

  def applyQueries(queryRequests: Seq[() => Seq[QueryConfig]], queryHandler: (Seq[Query]) => Seq[(Graph, Table)]): Boolean = {
    !queryRequests.exists(getter => {
      val configs = getter()
      val (queries, callbacks) = configs.map(c => (c.query, c.callback)).unzip
      if(queries.isEmpty)
        false
      else
        queryHandler(queries).zip(callbacks).exists { case ((g, t), f) => !f(g, t) }
    })
  }
}
