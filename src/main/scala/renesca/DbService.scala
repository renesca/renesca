package renesca

import renesca.graph._
import renesca.json.{ParameterValue, PropertyKey, PropertyValue}
import renesca.json.PropertyKey._

case class Query(statement:String, parameters:Map[PropertyKey, ParameterValue] = Map.empty)

object QueryHandler {
  private val graphChangeToQuery:GraphChange => Query = {
    case NodeSetProperty(nodeId, key, value) => Query("match (n) where id(n) = {id} set n += {keyValue}", Map("id" -> nodeId, "keyValue" -> Map(key -> value)))
    case NodeRemoveProperty(nodeId, key) => Query(s"match (n) where id(n) = {id} remove n.`$key`", Map("id" -> nodeId))
    case NodeSetLabel(nodeId, label) => Query(s"match (n) where id(n) = {id} set n:`${label.name}`", Map("id" -> nodeId))
    case NodeRemoveLabel(nodeId, label) => Query(s"match (n) where id(n) = {id} remove n:`${label.name}`", Map("id" -> nodeId))
    case NodeDelete(nodeId) => Query("match (n) where id(n) = {id} optional match (n)-[r]-() delete r,n", Map("id" -> nodeId))
    case RelationSetProperty(relationId, key, value) => Query("match ()-[r]->() where id(r) = {id} set r += {keyValue}", Map("id" -> relationId, "keyValue" -> Map(key -> value)))
    case RelationRemoveProperty(relationId, key) => Query(s"match ()-[r]->() where id(r) = {id} remove r.`$key`", Map("id" -> relationId))
    case RelationDelete(relationId) => Query("match ()-[r]->() where id(r) = {id} delete r", Map("id" -> relationId))
  }
}

trait QueryHandler {
  import QueryHandler._

  def queryGraph(statement:String, parameters:Map[PropertyKey, ParameterValue] = Map.empty):Graph = queryGraph(Query(statement, parameters))
  def queryGraph(query:Query):Graph = {
    val results = executeQueries(List(query), List("graph"))
    buildResults(results)
  }

  def batchQuery(statement:String, parameters:Map[PropertyKey, ParameterValue] = Map.empty):Unit = batchQuery(Query(statement, parameters))
  def batchQuery(query:Query) { executeQueries(List(query), Nil) }
  def batchQuery(queries:Seq[Query]) { executeQueries(queries, Nil) }

  def queryRows(query:String, parameters:Map[PropertyKey,PropertyValue]) = ???

  def persistChanges(graph:Graph) {
    val queries:Seq[Query] = graph.changes.map(graphChangeToQuery)
    batchQuery(queries)
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

  private def buildJsonRequest(queries:Seq[Query], resultDataContents:List[String]):json.Request = {
    json.Request(queries.map(query => json.Statement(query, resultDataContents)).toList)
  }

  private def handleError(jsonResponse:json.Response):List[json.Result] = {
    jsonResponse match {
      case json.Response(results, Nil) => results
      case json.Response(Nil, errors) =>
        val message = errors.map{ case json.Error(code, msg) => s"$code\n$msg"}.mkString("\n","\n\n","\n")
        throw new RuntimeException(message)
    }
  }

  protected def queryService(jsonRequest:json.Request):json.Response
}

class Transaction extends QueryHandler {
  var restService:RestService = null //TODO: inject

  //TODO: submit first query with openTransaction
  lazy val (id,_) = restService.openTransaction()

  override protected def queryService(jsonRequest:json.Request):json.Response = {
    restService.resumeTransaction(id, jsonRequest)
  }

  def commit() {
    restService.commitTransaction(id)
  }
}

class DbService extends QueryHandler {
  var restService:RestService = null //TODO: inject

  override protected def queryService(jsonRequest:json.Request):json.Response = {
    restService.singleRequest(jsonRequest)
  }
}




