package renesca

import renesca.graph.Graph
import renesca.json.{ParameterValue, PropertyValue}
object Query {
  implicit def StringToQuery(statement:String):Query = Query(statement)
}
case class Query(statement:String, parameters:Map[String, ParameterValue] = Map.empty)

class DbService {
  var restService:RestService = null

  def buildJsonRequest(query:Query, resultDataContents:List[String]):json.Request = {
    json.Request(List(json.Statement(query, resultDataContents)))
  }

  def buildJsonRequest(queries:Seq[Query]):json.Request = {
    json.Request(queries.map(json.Statement(_, Nil)).toList)
  }

  //TODO: error handling
  def batchQuery(query:Query) { batchQuery(List(query)) }
  def batchQuery(queries:Seq[Query]) {
    val jsonRequest = buildJsonRequest(queries)
    val jsonResponse = restService.awaitJsonResponse(jsonRequest)
  }

  def queryGraph(query:Query):Graph = {
    val jsonRequest = buildJsonRequest(query, List("graph"))
    val jsonResponse = restService.awaitJsonResponse(jsonRequest)

    val allJsonGraphs:Seq[json.Graph] = jsonResponse.results.flatMap{_.data.flatMap(_.graph)}
    val mergedGraph = allJsonGraphs.map(Graph(_)).fold(Graph())(_ merge _) //TODO: use Graph.empty
    mergedGraph
  }

  def queryRows(query:String, parameters:Map[String,PropertyValue]) = ???
}




