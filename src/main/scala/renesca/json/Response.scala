package renesca.json

import renesca.parameter.{ArrayParameterValue, PropertyMap}


case class Response(
                     commit: Option[String] = None,
                     results: List[Result] = Nil,
                     transaction: Option[Transaction] = None,
                     errors: List[Error] = Nil
                     )

case class Transaction(expires: String)

case class Error(code: String, message: String)

case class Data(row: Option[ArrayParameterValue] = None, graph: Option[Graph] = None)

case class Result(columns: List[String], data: List[Data])

case class Relationship(
                         id: String,
                         `type`: String,
                         startNode: String,
                         endNode: String,
                         properties: PropertyMap = Map.empty
                         )

case class Node(id: String, labels: List[String] = Nil, properties: PropertyMap = Map.empty)

case class Graph(nodes: List[Node] = Nil, relationships: List[Relationship] = Nil)

