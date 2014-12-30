package renesca

import renesca.graph.Graph
import renesca.json.protocols.ResponseJsonProtocol._
import spray.json._


// TODO: type of parameters? PorpertyValues?
case class Query(statement:String, parameters:Map[String,Any])
object QueryRequestType extends Enumeration {
  type QueryRequestType = Value
  val row, graph, rest = Value
}

import renesca.QueryRequestType._


class DbService {
  var restService:RestService = null

  def queryRequest(query:Query, queryRequestType:QueryRequestType):Request = {
    val jsonRequest = json.Request(List(json.Statement(query.statement, query.parameters, List(queryRequestType.toString))))
    // Request(POST, "/transaction/commit", Some(jsonRequest.toJson.compactPrint)
    Request(RequestType.POST, null, None)
  }

  def queryGraph(query:Query):Graph = {
    val request = queryRequest(query, QueryRequestType.graph)
    val jsonResponse = restService.submit(request).parseJson.convertTo[json.Response]
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




