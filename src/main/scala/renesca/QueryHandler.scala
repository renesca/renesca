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
import renesca.parameter.{ParameterValue, ParameterMap}
import renesca.parameter.implicits._
import renesca.table.Table

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
  def query(queries: Query*): Unit
  def persistChanges(graph: Graph): Unit
}

object QueryHandler {
  private val graphContentChangeToQuery: GraphContentChange => Query = {
    case NodeSetProperty(nodeId, key, value)         => Query("match (n) where id(n) = {id} set n += {keyValue}", Map("id" -> nodeId, "keyValue" -> Map(key -> value)))
    case NodeRemoveProperty(nodeId, key)             => Query(s"match (n) where id(n) = {id} remove n.`$key`", Map("id" -> nodeId))
    case NodeSetLabel(nodeId, label)                 => Query(s"match (n) where id(n) = {id} set n:`$label`", Map("id" -> nodeId))
    case NodeRemoveLabel(nodeId, label)              => Query(s"match (n) where id(n) = {id} remove n:`$label`", Map("id" -> nodeId))
    case NodeDelete(nodeId)                          => Query("match (n) where id(n) = {id} optional match (n)-[r]-() delete r,n", Map("id" -> nodeId))
    case RelationSetProperty(relationId, key, value) => Query("match ()-[r]->() where id(r) = {id} set r += {keyValue}", Map("id" -> relationId, "keyValue" -> Map(key -> value)))
    case RelationRemoveProperty(relationId, key)     => Query(s"match ()-[r]->() where id(r) = {id} remove r.`$key`", Map("id" -> relationId))
    case RelationDelete(relationId)                  => Query("match ()-[r]->() where id(r) = {id} delete r", Map("id" -> relationId))
  }

  private val graphStructureChangeToEffect: GraphStructureChange => QueryHandler => Graph => Unit = {
    case NodeAdd(localNodeId) => db => graph =>
      val dbNode = db.queryGraph("create (n) return n").nodes.head
      localNodeId.value = dbNode.id.value

    case RelationAdd(relationId, start, end, relationType) => db => graph =>
      val dbRelation = db.queryGraph(Query(
        s"match start,end where id(start) = {startId} and id(end) = {endId} create (start)-[r :`$relationType`]->(end) return r",
        Map("startId" -> start, "endId" -> end))).relations.head
      relationId.value = dbRelation.id.value
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

  def query(queries: Query*) { executeQueries(queries, Nil) }


  def persistChanges(graph: Graph) {
    //TODO: optimizations
    // - successive writes on property/label keep only the most recent one

    // produce changesets which end with a structural change
    val changeSets: List[List[GraphChange]] = graph.changes.foldRight(List(List.empty[GraphChange])) {
      case (x: GraphStructureChange, xs: List[List[GraphChange]]) => List(x) :: xs
      case (x: GraphChange, xs: List[List[GraphChange]])          => (x :: xs.head) :: xs.tail
    }

    for(changeSet <- changeSets) {
      // fire one query for all content changes (properties/labels)
      val contentChanges = changeSet collect { case c: GraphContentChange => c }
      val contentChangeQueries: Seq[Query] = contentChanges.map(graphContentChangeToQuery)
      query(contentChangeQueries: _*)

      // fire queries for each structural change (add nodes/relations, ...)
      val structuralChanges = changeSet collect { case c: GraphStructureChange => c }
      for(structuralChange <- structuralChanges) {
        graphStructureChangeToEffect(structuralChange)(this)(graph)
      }
    }

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
        val message = errors.map { case json.Error(code, msg) => s"$code\n$msg" }.mkString("\n", "\n\n", "\n")
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
      for(transactionId <- id)
        restService.commitTransaction(transactionId)

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
}




