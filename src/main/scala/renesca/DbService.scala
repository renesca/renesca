package renesca

import renesca.graph.Graph
import renesca.json.{ParameterValue, PropertyValue}

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
  def batchQuery(queries:Seq[Query]) {
    val jsonRequest = buildJsonRequest(queries)
    val jsonResponse = restService.awaitJsonResponse(jsonRequest)
  }

  def queryGraph(statement:String):Graph = queryGraph(Query(statement))

  def queryGraph(query:Query):Graph = {
    val jsonRequest = buildJsonRequest(query, List("graph"))
    val jsonResponse = restService.awaitJsonResponse(jsonRequest)

    val allJsonGraphs:Seq[json.Graph] = jsonResponse.results.flatMap{_.data.flatMap(_.graph)}
    val mergedGraph = if(allJsonGraphs.nonEmpty) allJsonGraphs.map(Graph(_)).reduce(_ merge _) else Graph()
    mergedGraph
  }

  def queryRows(query:String, parameters:Map[String,PropertyValue]) = ???
}




