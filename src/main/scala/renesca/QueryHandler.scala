package renesca

// QueryHandler ensures that we have the same query-interface in Transaction and in DbService.
// Both must implement queryService and handleError.
// The query-interface consists of the methods:
//  queryGraph  - submit one query [with parameters] and return a Graph
//  queryTable  - submit one query [with parameters] and return a row set
//  queryGraphs - submit multiple queries [with parameters] and return a Graph for each result
//  queryTables - submit multiple queries [with parameters] and return a row set for each result
//  query       - submit one or multiple queries [with parameters] as a side effect (returns Unit)
//  persist     - save a modified graph to the database

import renesca.graph._
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.table.Table

import scala.collection.mutable

object Query {
  implicit def stringToQuery(statement: String): Query = Query(statement)
}

case class Query(statement: String, parameters: ParameterMap = Map.empty)

trait QueryInterface {
  def queryWholeGraph: Graph
  def queryGraph(query: Query): Graph
  def queryTable(query: Query): Table
  def queryGraphs(queries: Query*): Seq[Graph]
  def queryTables(queries: Query*): Seq[Table]
  def queryGraphsAndTables(queries: Query*): Seq[(Graph,Table)]
  def query(queries: Query*): Unit
  def persistChanges(graph: Graph): Unit
  def persistChanges(schemaGraph: schema.Graph): Unit = persistChanges(schemaGraph.graph)
}

private object QueryHandler {
  def selectLiteralMap(properties: Properties, selection: Set[PropertyKey]) = {
    val variable = randomVariable
    val remainingProperties = properties.filterKeys(!selection.contains(_))
    val selectedProperties = properties.filterKeys(selection.contains(_))
    val parameterMap = selectedProperties.toMap.map { case (k, v) => (PropertyKey(s"$k$variable"), v) }
    val literalMap = selectedProperties.map { case (k, v) => s"$k: {$k$variable}" }
    val literalMapMatcher = if (literalMap.isEmpty) "" else literalMap.mkString("{", ",", "}")
    (literalMapMatcher, remainingProperties, parameterMap)
  }

  def randomVariable = "V" + java.util.UUID.randomUUID().toString.replace("-", "")

  def queryNonLocalNode(node: Node): (String, ParameterMap, String) = {
    val variable = randomVariable
    (s"match ($variable) where id($variable) = {nodeId}", Map("nodeId" -> node.id), variable)
  }

  def queryLocalNode(node: Node): (String, ParameterMap, String) = {
    val labels = node.labels.map(label => s":`$label`").mkString
    val variable = randomVariable
    node.origin match {
      case Create()          =>
        (s"create ($variable $labels {properties$variable})", Map(s"properties$variable" -> node.properties.toMap), variable)
      case Merge(merge, onMatch) =>
        val (mergeLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(node.properties, merge.toSet)
        val onMatchProperties = remainingProperties.filterKeys(onMatch contains _)
        (s"merge ($variable $labels $mergeLiteralMap) on create set $variable += {createProperties$variable} on match set $variable += {onMatchProperties$variable}",
          parameterMap ++ Map(s"createProperties$variable" -> remainingProperties.toMap, s"onMatchProperties$variable" -> onMatchProperties.toMap),
          variable)
      case Match()           =>
        val (matchLiteralMap, _, parameterMap) = selectLiteralMap(node.properties, node.properties.keySet.toSet)
        (s"match ($variable $labels $matchLiteralMap)", parameterMap, variable)
    }
  }

  def queryLocalRelation(relation: Relation): (String, ParameterMap, String) = {
    val variable = randomVariable
    val (creatorStr, parameters) = relation.origin match {
      case Create()          =>
        (s"create (start$variable)-[$variable :`${ relation.relationType }` {properties$variable}]->(end$variable)", Map(s"properties$variable" -> relation.properties.toMap))
      case Merge(merge, onMatch) =>
        val (mergeLiteralMap, remainingProperties, parameterMap) = selectLiteralMap(relation.properties, merge.toSet)
        val onMatchProperties = remainingProperties.filterKeys(onMatch contains _)
        (s"merge (start$variable)-[$variable :`${ relation.relationType }` $mergeLiteralMap]->(end$variable) on create set $variable += {createProperties$variable} on match set $variable += {onMatchProperties$variable}",
          parameterMap ++ Map(s"createProperties$variable" -> remainingProperties.toMap, s"onMatchProperties$variable" -> onMatchProperties.toMap))
      case Match()           =>
        val (matchLiteralMap, _, parameterMap) = selectLiteralMap(relation.properties, relation.properties.keySet.toSet)
        (s"match (start$variable)-[$variable :`${ relation.relationType }` $matchLiteralMap]->(end$variable)", parameterMap)
    }

    val queryStr = s"match (start$variable),(end$variable) where id(start$variable) = {startId$variable} and id(end$variable) = {endId$variable} $creatorStr"
    val parameterMap = parameters ++ Map(s"startId$variable" -> relation.startNode.id, s"endId$variable" -> relation.endNode.id)
    (queryStr, parameterMap, variable)
  }

  def makeQueryWithReturn(queryStr: String, parameters: ParameterMap, variable: String): Query = {
    Query(s"$queryStr return $variable", parameters)
  }

  def makeQueryWithReturn(tuple: (String, ParameterMap, String)): Query = makeQueryWithReturn(tuple._1, tuple._2, tuple._3)

  def addNodesToQueries(addNodes: Seq[Node]) = {
    addNodes.map(node => {
      val query = makeQueryWithReturn(queryLocalNode(node))

      (query, (graph: Graph, table: Table) => {
        val dbNodeOpt = graph.nodes.headOption
        //TODO: what if node does not exist? remove from graph?
        dbNodeOpt.foreach(dbNode => {
          node.properties ++= dbNode.properties
          node.labels ++= dbNode.labels
          node.id.value = dbNode.id.value
        })
      })
    })
  }

  def addRelationsToQueries(addRelations: Seq[Relation]) = {
    addRelations.map(relation => {
      val query = makeQueryWithReturn(queryLocalRelation(relation))

      (query, (graph: Graph, table: Table) => {
        val dbRelationOpt = graph.relations.headOption
        //TODO: what if relation does not exist? remove from graph?
        dbRelationOpt.foreach(dbRelation =>{
          relation.properties ++= dbRelation.properties
          relation.id.value = dbRelation.id.value
        })
      })
    })
  }

  def nodePattern(node: Node) = {
    val variable = randomVariable
    val labels = node.labels.map(label => s":`$label`").mkString
    val (literalMap, _, parameterMap) = selectLiteralMap(node.properties, node.properties.keySet.toSet)
    (s"($variable $labels $literalMap)", parameterMap, variable)
  }

  def relationPattern(relation: Relation) = {
    val variable = randomVariable
    val (literalMap, _, parameterMap) = selectLiteralMap(relation.properties, relation.properties.keySet.toSet)
    (s"[$variable : `${relation.relationType}` $literalMap]", parameterMap, variable)
  }

  def addPathsToQueries(addPaths: Seq[Path]) = {
    addPaths.map(path => {
      val existingNodes = path.nodes.filterNot(_.id.isLocal)
      val localNodes = path.nodes.filter(_.id.isLocal)
      val boundNodes = localNodes.filterNot(_.origin == path.origin)

      val (preQueries, preParameterMaps, preVariables) = (existingNodes.map(queryNonLocalNode(_)) ++ boundNodes.map(queryLocalNode(_))).unzip3
      val preVariableMap = ((existingNodes ++ boundNodes) zip preVariables).toMap
      val preQuery = preQueries.mkString(" ")

      val (pathQueries, parameterMaps, variableMaps) = path.relations.map(relation => {
        val (startQuery, startParameters, startVariable) = preVariableMap.get(relation.startNode).map(variable => (s"($variable)", Map.empty, variable)).getOrElse(nodePattern(relation.startNode))
        val (endQuery, endParameters, endVariable) = preVariableMap.get(relation.endNode).map(variable => (s"($variable)", Map.empty, variable)).getOrElse(nodePattern(relation.endNode))
        val (relQuery, relParameters, relVariable) = relationPattern(relation)
        (s"$startQuery-$relQuery->$endQuery", startParameters ++ endParameters ++ relParameters, Map(
          relation.startNode -> startVariable, relation.endNode -> endVariable, relation -> relVariable
        ))
      }).unzip3

      val parameterMap = (preParameterMaps ++ parameterMaps).flatten.toMap
      val variableMap = preVariableMap ++ (variableMaps.flatten.toMap)
      val reverseVariableMap = variableMap.map { case (k, v) => (v, k) }
      val pathPattern = pathQueries.mkString(" ")
      val pathQuery = path.origin match {
        case Match() => s"match $pathPattern"
        case Merge(merge, onMatch) => s"merge $pathPattern"
      }

      val returnClause = "return " + variableMap.map {
        case (k,variable) => k match {
          case _: Node => s"{id: id($variable), properties: $variable, labels: labels($variable)}"
          case _: Relation => s"{id: id($variable), properties: $variable}"
        }
      }.mkString(",")

      val queryStr = s"$preQuery $pathQuery $returnClause"

      (Query(queryStr, parameterMap), (graph: Graph, table: Table) => {
        table.columns.foreach(col => {
          table.rows.headOption.foreach(row => {
            val map = row(col).asInstanceOf[MapParameterValue].value
            reverseVariableMap(col) match {
              case n: Node =>
                n.properties.clear
                n.labels.clear
                n.properties ++= map("properties").asInstanceOf[PropertyMap]
                n.labels ++= map("labels").asInstanceOf[ArrayParameterValue].value.asInstanceOf[Seq[Label]]
                n.id.value = map("id").asInstanceOf[Long]
              case r: Relation =>
                r.properties.clear()
                r.properties ++= map("properties").asInstanceOf[PropertyMap]
                r.id.value = map("id").asInstanceOf[Long]
            }
          })
        })
      })
    })
  }

  def contentChangesToQueries(contentChanges: Seq[GraphContentChange]) = {
    contentChanges.groupBy(_.item).map {
      case (item, changes) =>
        val propertyAdditions: mutable.Map[PropertyKey, ParameterValue] = mutable.Map.empty
        val propertyRemovals: mutable.Set[PropertyKey] = mutable.Set.empty
        val labelAdditions: mutable.Set[Label] = mutable.Set.empty
        val labelRemovals: mutable.Set[Label] = mutable.Set.empty
        val deleteOpt = changes.find {
          case SetProperty(_, key, value) =>
            propertyRemovals -= key
            propertyAdditions += key -> value
            false
          case RemoveProperty(_, key)     =>
            propertyRemovals += key
            propertyAdditions -= key
            false
          case SetLabel(_, label)         =>
            labelRemovals -= label
            labelAdditions += label
            false
          case RemoveLabel(_, label)      =>
            labelRemovals += label
            labelAdditions -= label
            false
          case DeleteItem(_)              =>
            true
        }

        val isRelation = item match {
          case _: Node     => false
          case _: Relation => true
        }

        val variable = "n"
        val matcher = if(isRelation) s"match ()-[$variable]->()" else s"match ($variable)"
        val setters = deleteOpt.map { _ =>
          if(isRelation) {
            s"delete $variable"
          } else {
            val optionalVariable = "m"
            s"optional match ($variable)-[$optionalVariable]-() delete $optionalVariable, $variable"
          }
        }.getOrElse {
          val propertyRemove = propertyRemovals.map(r => s"remove $variable .`$r`").mkString(" ")
          val labelAdd = labelAdditions.map(a => s"set $variable :`$a`").mkString(" ")
          val labelRemove = labelRemovals.map(r => s"remove $variable :`$r`").mkString(" ")
          s"set $variable += {propertyAdditions} $propertyRemove $labelAdd $labelRemove"
        }

        (Query(
          s"$matcher where id($variable) = {itemId} $setters",
          Map("itemId" -> item.id, "propertyAdditions" -> propertyAdditions.toMap)
        ), (graph: Graph, table: Table) => {})
    }.toSeq
  }

  def applyQueries(queriesWithCallbacks: Seq[(Query, (Graph, Table) => Unit)], queryHandler: QueryHandler): Unit = {
    val queries = queriesWithCallbacks.map(_._1)
    val callbacks = queriesWithCallbacks.map(_._2)
    queryHandler.queryGraphsAndTables(queries: _*).zip(callbacks).foreach { case ((g, t), f) => f(g, t) }
  }

}

trait QueryHandler extends QueryInterface {

  import renesca.QueryHandler._

  override def queryWholeGraph: Graph = queryGraph("match (n) optional match (n)-[r]-() return n,r")
  override def queryGraph(query: Query): Graph = queryGraphs(query).head
  override def queryTable(query: Query): Table = queryTables(query).head

  override def queryGraphs(queries: Query*): Seq[Graph] = {
    val results = executeQueries(queries, List("graph"))
    extractGraphs(results)
  }

  override def queryTables(queries: Query*): Seq[Table] = {
    val results = executeQueries(queries, List("row"))
    extractTables(results)
  }

  override def queryGraphsAndTables(queries: Query*): Seq[(Graph,Table)] = {
    val results = executeQueries(queries, List("row", "graph"))
    extractGraphs(results) zip extractTables(results)
  }

  def query(queries: Query*) { executeQueries(queries, Nil) }

  def persistChanges(graph: Graph) {
    val changes = graph.changes
    val contentChanges = changes.collect { case c: GraphContentChange => c}
    val addPaths = changes.collect { case AddPath(p) => p}

    // all addItem changes that are already included in paths need to be ignored,
    // as they are handled when adding the path itself
    val pathItems = addPaths.flatMap(path => path.nodes ++ path.relations)
    val addItemChanges = changes.collect { case c: AddItem => c}.filterNot(pathItems contains _.item)

    val contentQueries = contentChangesToQueries(contentChanges)
    val addNodeQueries = addNodesToQueries(addItemChanges.collect { case AddItem(n: Node) => n })
    val addPathQueries = addPathsToQueries(addPaths)
    applyQueries(contentQueries ++ addNodeQueries /*++ addPathQueries*/, this)

    //TODO: merge relation queries into previous request by referring to variables instead of ids
    val addRelationQueries = addRelationsToQueries(addItemChanges.collect { case AddItem(r: Relation) => r })
    applyQueries(addRelationQueries, this)

    graph.clearChanges()
  }

  protected def executeQueries(queries: Seq[Query], resultDataContents: List[String]): List[json.Result] = {
    val jsonRequest = buildJsonRequest(queries, resultDataContents)
    val jsonResponse = queryService(jsonRequest)
    handleError(exceptionFromErrors(jsonResponse))
    jsonResponse.results
  }

  protected def extractGraphs(results: Seq[json.Result]): Seq[Graph] = {
    val allJsonGraphs: Seq[List[json.Graph]] = results.map(_.data.flatMap(_.graph))
    allJsonGraphs.map(_.map(Graph(_)).fold(Graph.empty)(_ merge _))
  }

  protected def extractTables(results: Seq[json.Result]): Seq[Table] = {
    results.map(r => Table(r))
  }

  protected def buildJsonRequest(queries: Seq[Query], resultDataContents: List[String]): json.Request = {
    json.Request(queries.map(query => json.Statement(query, resultDataContents)).toList)
  }

  protected def exceptionFromErrors(jsonResponse: json.Response): Option[RuntimeException] = {
    jsonResponse.errors match {
      case Nil    => None
      case errors =>
        val message = errors.map {
          case json.Error(code, msg) => s"$code\n$msg"
        }.mkString("\n", "\n\n", "\n")
        Some(new RuntimeException(message))
    }
  }

  protected def queryService(jsonRequest: json.Request): json.Response
  protected def handleError(exceptions: Option[Exception]): Unit
}

class Transaction extends QueryHandler {thisTransaction =>

  var restService: RestService = null //TODO: inject

  var id: Option[TransactionId] = None

  private var valid = true
  def isValid = valid
  def invalidate() { valid = false }
  private def throwIfNotValid() {
    if(!valid)
      throw new RuntimeException("Transaction is not valid anymore.")
  }


  override protected def queryService(jsonRequest: json.Request): json.Response = {
    throwIfNotValid()
    id match {
      case Some(transactionId) => restService.resumeTransaction(transactionId, jsonRequest)
      case None                =>
        val (transactionId, jsonResponse) = restService.openTransaction(jsonRequest)
        id = Some(transactionId)
        jsonResponse
    }
  }

  protected def handleError(exceptions: Option[Exception]) {
    for(exception <- exceptions) {
      rollback()
      throw exception
    }
  }

  def rollback() = {
    id match {
      case Some(transactionId) => restService.rollbackTransaction(transactionId)
      case None                =>
    }
    invalidate()
  }

  val commit = new QueryHandler {
    // Important:
    // queryService can be called only once, because it commits and invalidates the transaction.
    // This means that methods like persistChanges which are firing multiple queries and thus calling querySerice
    // multiple times need to be modified or wrapped.

    def apply() {
      throwIfNotValid()
      for(transactionId <- id) {
        val jsonResponse = restService.commitTransaction(transactionId)
        handleError(exceptionFromErrors(jsonResponse))
      }

      invalidate()
    }

    override protected def queryService(jsonRequest: json.Request): json.Response = {
      // TODO: share code with this.Transaction.queryService
      throwIfNotValid()
      val jsonResponse = id match {
        case Some(transactionId) => restService.commitTransaction(transactionId, jsonRequest)
        case None                => restService.singleRequest(jsonRequest)
      }

      invalidate()
      jsonResponse
    }

    override def persistChanges(graph: Graph) {
      //TODO: this solution can send one REST request more than needed, as the commit request does not do any changes
      thisTransaction.persistChanges(graph)
      apply() // commit
    }


    override protected def handleError(exceptions: Option[Exception]) = thisTransaction.handleError(exceptions)
  }

  override def toString = s"Transaction(id=$id, valid:$isValid, $restService)"
}

class DbService extends QueryHandler {
  var restService: RestService = null //TODO: inject

  protected def handleError(exceptions: Option[Exception]) {
    for(exception <- exceptions)
      throw exception
  }

  override protected def queryService(jsonRequest: json.Request): json.Response = {
    restService.singleRequest(jsonRequest)
  }

  def newTransaction() = {
    val tx = new Transaction
    tx.restService = restService
    tx
  }

  def transaction(code: Transaction => Any): Unit = {
    val tx = newTransaction
    code(tx)

    // if there was a request and transacion is not done yet
    if(tx.id.isDefined && tx.isValid) tx.commit()
  }
}
