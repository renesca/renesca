package renesca

import renesca.graph._
import renesca.parameter.ParameterMap
import renesca.parameter.implicits._

case class Query(statement:String, parameters:ParameterMap = Map.empty)

object QueryHandler {
  private val graphContentChangeToQuery:GraphContentChange => Query = {
    case NodeSetProperty(nodeId, key, value) => Query("match (n) where id(n) = {id} set n += {keyValue}", Map("id" -> nodeId, "keyValue" -> Map(key -> value)))
    case NodeRemoveProperty(nodeId, key) => Query(s"match (n) where id(n) = {id} remove n.`$key`", Map("id" -> nodeId))
    case NodeSetLabel(nodeId, label) => Query(s"match (n) where id(n) = {id} set n:`$label`", Map("id" -> nodeId))
    case NodeRemoveLabel(nodeId, label) => Query(s"match (n) where id(n) = {id} remove n:`$label`", Map("id" -> nodeId))
    case NodeDelete(nodeId) => Query("match (n) where id(n) = {id} optional match (n)-[r]-() delete r,n", Map("id" -> nodeId))
    case RelationSetProperty(relationId, key, value) => Query("match ()-[r]->() where id(r) = {id} set r += {keyValue}", Map("id" -> relationId, "keyValue" -> Map(key -> value)))
    case RelationRemoveProperty(relationId, key) => Query(s"match ()-[r]->() where id(r) = {id} remove r.`$key`", Map("id" -> relationId))
    case RelationDelete(relationId) => Query("match ()-[r]->() where id(r) = {id} delete r", Map("id" -> relationId))
  }

  private val graphStructureChangeToEffect:GraphStructureChange => QueryHandler => Graph => Unit = {
    case NodeAdd(localNodeId, labels, properties) => db => graph =>
      val labelDef = labels.map(l => s":`$l`").mkString
      val dbNode = db.queryGraph(s"create (n $labelDef) set n += {keyValue} return n", Map("keyValue" -> properties)).nodes.head
      localNodeId.value = dbNode.id.value
    case RelationAdd(relationId, start, end, relationType, properties) => db => graph =>
      val dbRelation = db.queryGraph(s"match start,end where id(start) = {startId} and id(end) = {endId} create (start)-[r :`$relationType`]->(end) set r += {keyValue} return r", Map("startId" -> start, "endId" -> end, "keyValue" -> properties)).relations.head
      relationId.value = dbRelation.id.value
  }
}

trait QueryHandler {
  import renesca.QueryHandler._

  def queryGraph(statement:String, parameters:ParameterMap = Map.empty):Graph = queryGraph(Query(statement, parameters))
  def queryGraph(query:Query):Graph = {
    val results = executeQueries(List(query), List("graph"))
    buildResults(results)
  }

  def batchQuery(statement:String, parameters:ParameterMap = Map.empty):Unit = batchQuery(Query(statement, parameters))
  def batchQuery(query:Query) { executeQueries(List(query), Nil) }
  def batchQuery(queries:Seq[Query]) { executeQueries(queries, Nil) }

  def queryRows(query:String, parameters:ParameterMap) = ???

  def persistChanges(graph:Graph) {
    //TODO: optimizations
    // - successive writes on property/label keep only the most recent one

    // produce changesets which end with a structural change
    val changeSets:List[List[GraphChange]] = graph.changes.foldRight(List(List.empty[GraphChange])) {
      case (x:GraphStructureChange,xs:List[List[GraphChange]]) => List(x) :: xs
      case (x:GraphChange,          xs:List[List[GraphChange]]) => (x :: xs.head) :: xs.tail
    }

    for( changeSet <- changeSets ) {
      val contentChanges = changeSet collect {case c:GraphContentChange => c }
      val structuralChanges = changeSet collect {case c:GraphStructureChange => c }

      val contentChangeQueries:Seq[Query] = contentChanges.map(graphContentChangeToQuery)
      batchQuery(contentChangeQueries)

      for(structuralChange <- structuralChanges) {
        graphStructureChangeToEffect(structuralChange)(this)(graph)
      }
    }

    graph.clearChanges()
  }

  protected def executeQueries(queries:Seq[Query], resultDataContents:List[String]):List[json.Result] = {
    val jsonRequest = buildJsonRequest(queries, resultDataContents)
    val jsonResponse = queryService(jsonRequest)
    val results = handleError(jsonResponse)
    results
  }

  protected def buildResults(results:Seq[json.Result]):Graph = {
    val allJsonGraphs:Seq[json.Graph] = results.flatMap{_.data.flatMap(_.graph)}
    val mergedGraph = allJsonGraphs.map(Graph(_)).fold(Graph())(_ merge _) //TODO: use Graph.empty
    mergedGraph
  }

  protected def buildJsonRequest(queries:Seq[Query], resultDataContents:List[String]):json.Request = {
    json.Request(queries.map(query => json.Statement(query, resultDataContents)).toList)
  }

  protected def handleError(jsonResponse:json.Response):List[json.Result] = {
    jsonResponse match {
      case json.Response(_, results, _, Nil) => results
      case json.Response(_, Nil    , _, errors) =>
        val message = errors.map{ case json.Error(code, msg) => s"$code\n$msg"}.mkString("\n","\n\n","\n")
        throw new RuntimeException(message)
    }
  }

  protected def queryService(jsonRequest:json.Request):json.Response
}

class Transaction extends QueryHandler {

  var restService:RestService = null //TODO: inject

  var id:Option[TransactionId] = None

  private var valid = true
  def isValid = valid
  def invalidate() {valid = false}
  private def throwIfNotValid() {
    if(!valid)
      throw new RuntimeException("Transaction is not valid anymore.")
  }


  override protected def queryService(jsonRequest:json.Request):json.Response = {
    throwIfNotValid()
    id match {
      case Some(transactionId) => restService.resumeTransaction(transactionId, jsonRequest)
      case None =>
        val (transactionId, jsonResponse) = restService.openTransaction(jsonRequest)
        id = Some(transactionId)
        jsonResponse
    }
  }

  def commit() {
    throwIfNotValid()
    for( transactionId <- id)
      restService.commitTransaction(transactionId)

    invalidate()
  }

  def commit(statement:String, parameters:ParameterMap = Map.empty):Graph = {
    commit(Query(statement, parameters))
  }

  def commit(query:Query):Graph = {
    throwIfNotValid()
    val jsonRequest = buildJsonRequest(List(query), List("graph"))
    val jsonResponse = id match {
      case Some(transactionId) => restService.commitTransaction(transactionId, jsonRequest)
      case None =>                restService.singleRequest(jsonRequest)
    }

    invalidate()
    buildResults(handleError(jsonResponse))
  }
}

class DbService extends QueryHandler {
  var restService:RestService = null //TODO: inject

  override protected def queryService(jsonRequest:json.Request):json.Response = {
    restService.singleRequest(jsonRequest)
  }
}




