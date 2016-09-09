package renesca.json

import renesca.parameter.{ArrayParameterValue, PropertyMap}
import renesca.graph
import renesca.table

import renesca.json

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

object GraphFactory {
  def apply(jsonGraph: json.Graph): graph.Graph = {
    val nodes: List[graph.Node] = jsonGraph.nodes.map {
      case json.Node(id, labels, properties) =>
        graph.Node(graph.Id(id.toLong), labels.map(graph.Label.apply), properties)
    }

    val idToNode: Map[String, graph.Node] = nodes.flatMap(node => node.origin match {
      case graph.Id(id) => Map(id.toString -> node)
      case _ => Map.empty[String, graph.Node]
    }).toMap

    val relations: List[graph.Relation] = jsonGraph.relationships.map {
      case json.Relationship(id, relationshipType, startNode, endNode, properties) =>
        graph.Relation(
          graph.Id(id.toLong),
          idToNode(startNode),
          idToNode(endNode),
          graph.RelationType(relationshipType),
          properties
        )
    }

    graph.Graph(nodes, relations)
  }
}

object TableFactory {
  def apply(result: json.Result): table.Table = {
    val data = result.data.flatMap { _.row.map(_.value) }
    table.Table(result.columns, data)
  }
}
