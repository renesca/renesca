package renesca.json

case class Response(results : List[Result] = Nil, errors : List[Error] = Nil)
case class Relationship(
                         id : String,
                         `type` : String,
                         startNode:String,
                         endNode:String,
                         properties : Map[String, Value] = Map.empty
                         )
case class Node(id : String, labels : List[String] = Nil, properties : Map[String, Value] = Map.empty)
case class Graph(nodes : List[Node] = Nil, relationships : List[Relationship] = Nil)
case class Error(code : String, messages : String)
case class Data(row : Option[RowData] = None, graph : Option[Graph] = None)
case class RowData()
case class Result(columns : List[String], data : List[Data])

