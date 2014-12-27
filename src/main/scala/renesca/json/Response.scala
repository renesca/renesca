package renesca.json

import spray.json.JsValue

case class Response(results : List[Result] = Nil, errors : List[Error] = Nil)
case class Relationship(
                         id : String,
                         `type` : String,
                         startNode:String,
                         endNode:String,
                         properties : Map[String, JsValue] = Map.empty //TODO: PropertyValue
                         )
case class Node(id : String, labels : List[String] = Nil, properties : Map[String, JsValue] = Map.empty)
case class GraphData(nodes : List[Node] = Nil, relationships : List[Relationship] = Nil)
case class Error(code : String, messages : String)
case class Data(row : Option[RowData] = None, graph : Option[GraphData] = None)
case class RowData()
case class Result(columns : List[String], data : List[Data])

