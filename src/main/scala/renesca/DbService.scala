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
    ???
  }

  def queryGraph(query:Query):Graph = {
    val request = queryRequest(query, QueryRequestType.graph)
    val jsonResponse = restService.submit(request).parseJson.convertTo[json.Response]
    val jsonGraph = ???

    Graph(jsonGraph)
  }

  def queryRows(query:String, parameters:Map[String,Any]) = ???
}




