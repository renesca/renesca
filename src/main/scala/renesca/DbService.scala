package renesca

import renesca.graph.Graph
import renesca.json.{PropertyKey, ParameterValue, PropertyValue}

case class Query(statement:String, parameters:Map[PropertyKey, ParameterValue] = Map.empty)

trait QueryHandler {
  def queryGraph(statement:String, parameters:Map[PropertyKey, ParameterValue] = Map.empty):Graph = queryGraph(Query(statement, parameters))
  def queryGraph(query:Query):Graph = {
    val results = executeQueries(List(query), List("graph"))
    buildResults(results)
  }
  def batchQuery(statement:String, parameters:Map[PropertyKey, ParameterValue] = Map.empty):Unit = batchQuery(Query(statement, parameters))
  def batchQuery(query:Query) { executeQueries(List(query), Nil) }
  def batchQuery(queries:Seq[Query]) { executeQueries(queries, Nil) }
  def queryRows(query:String, parameters:Map[PropertyKey,PropertyValue]) = ???

  protected def executeQueries(queries:Seq[Query], resultDataContents:List[String]):List[json.Result]
  protected def buildResults(results:Seq[json.Result]):Graph
}

class DbService extends QueryHandler {
  var restService:RestService = null //TODO: inject

  protected def buildResults(results:Seq[json.Result]):Graph = {
    val allJsonGraphs:Seq[json.Graph] = results.flatMap{_.data.flatMap(_.graph)}
    val mergedGraph = allJsonGraphs.map(Graph(_)).fold(Graph())(_ merge _) //TODO: use Graph.empty
    mergedGraph
  }

  protected def executeQueries(queries:Seq[Query], resultDataContents:List[String]):List[json.Result] = {
    val jsonRequest = buildJsonRequest(queries, resultDataContents)
    val jsonResponse = restService.awaitJsonResponse(jsonRequest)
    val results = handleError(jsonResponse)
    results
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
}




