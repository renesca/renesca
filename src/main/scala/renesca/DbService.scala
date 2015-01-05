package renesca

import renesca.graph.Graph
import renesca.json.Value

case class Query(statement:String, parameters:Map[String,Value] = Map.empty)


class DbService {
  var restService:RestService = null

  def buildJsonRequest(query:Query, queryRequestType:List[String]):json.Request = {
    json.Request(List(
      json.Statement(
        query.statement,
        if (query.parameters.nonEmpty) Some(query.parameters) else None,
        Some(queryRequestType)
      )
    ))
  }


  def queryGraph(query:Query):Graph = {
    val jsonRequest = buildJsonRequest(query, List("graph"))
    val jsonResponse = restService.awaitJsonResponse(jsonRequest)

    val allJsonGraphs:Seq[json.Graph] = jsonResponse.results.flatMap{_.data.flatMap(_.graph)}
    val mergedGraph = allJsonGraphs.map(Graph(_)).reduce(_ merge _)
    mergedGraph
  }

  def queryRows(query:String, parameters:Map[String,Value]) = ???
}




