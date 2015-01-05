package renesca

import renesca.graph.Graph

case class Query(statement:String, parameters:Map[String,String] = Map.empty)


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

    var graph = Graph()
    jsonResponse.results.foreach(result => {
      result.data.foreach (data => {
        data.graph.map(jsonGraph => {
          graph = graph merge Graph(jsonGraph)
        })
      })
    })
    graph
  }

  def queryRows(query:String, parameters:Map[String,Any]) = ???
}




