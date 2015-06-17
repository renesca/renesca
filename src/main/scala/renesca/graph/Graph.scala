package renesca.graph

import renesca.json
import renesca.parameter.PropertyKey

import scala.collection.mutable

object Graph {
  def apply(nodes: Traversable[Node], relations: Traversable[Relation] = Nil): Graph = {
    new Graph(
      new Nodes(mutable.LinkedHashSet.empty ++ nodes),
      new Relations(mutable.LinkedHashSet.empty ++ relations)
    )
  }

  def apply(jsonGraph: json.Graph): Graph = {
    val nodes: List[Node] = jsonGraph.nodes.map { case json.Node(id, labels, properties) =>
      Node(Id(id.toLong), labels.map(Label.apply), properties)
    }

    val idToNode: Map[String, Node] = nodes.flatMap (node => node.origin match {
      case Id(id) => Map(id.toString -> node)
      case _      => Map.empty[String, Node]
    }).toMap

    val relations: List[Relation] = jsonGraph.relationships.map {
      case json.Relationship(id, relationshipType, startNode, endNode, properties) =>
        Relation(Id(id.toLong),
          idToNode(startNode),
          idToNode(endNode),
          RelationType(relationshipType),
          properties)
    }

    Graph(nodes, relations)
  }

  def empty = new Graph(new Nodes, new Relations)
}

class Graph private[graph](val nodes: Nodes, val relations: Relations) {
  // private constructor to force usage of Factory

  nodes.graph = this // TODO: is it possible to eliminate this cyclic reference? (used for removing node incident relations)
  relations.graph = this // TODO: is it possible to eliminate this cyclic reference? (used for adding nonexistent start/endNode to graph)

  // graph must be consistent
  require(relations.forall { relation =>
    (nodes contains relation.startNode) &&
      (nodes contains relation.endNode)
  })
  // TODO: test

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  def +=(path: Path): Unit = {
    localChanges += AddPath(path)
    nodes ++= path.allNodes
    relations ++= path.relations
  }

  def changes: Seq[GraphChange] = {
    localChanges ++
      nodes.localChanges ++
      relations.localChanges ++
      nodes.toSeq.flatMap(_.changes) ++
      relations.toSeq.flatMap(_.changes)
  }

  def clearChanges() {
    localChanges.clear()
    nodes.foreach { node =>
      node.properties.localChanges.clear()
      node.labels.localChanges.clear()
    }
    relations.foreach { relation =>
      relation.properties.localChanges.clear()
    }
    nodes.localChanges.clear()
    relations.localChanges.clear()
  }

  def outRelations(node: Node) = relations.filter(node == _.startNode).toSet
  def inRelations(node: Node) = relations.filter(node == _.endNode).toSet
  def incidentRelations(node: Node): Set[Relation] = inRelations(node) ++ outRelations(node)
  def neighbours(node: Node) = incidentRelations(node).map(_.other(node))
  def successors(node: Node) = outRelations(node).map(_.endNode)
  def predecessors(node: Node) = inRelations(node).map(_.startNode)
  def inDegree(node: Node) = inRelations(node).size
  def outDegree(node: Node) = outRelations(node).size
  def degree(node: Node) = inDegree(node) + outDegree(node)

  def merge(that: Graph) = {
    val graph = Graph(this.nodes ++ that.nodes, this.relations ++ that.relations)
    graph.localChanges ++= this.changes
    graph.localChanges ++= that.changes
    graph
  }

  def isEmpty = nodes.isEmpty
  def nonEmpty = nodes.nonEmpty

  override def toString = s"Graph(nodes:(${ nodes.mkString(", ") }), relations:(${ relations.mkString(", ") }))"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Graph]

  override def equals(other: Any): Boolean = other match {
    case that: Graph =>
      (that canEqual this) &&
        this.nodes == that.nodes &&
        this.relations == that.relations
    case _           => false
  }

  override def hashCode(): Int = {
    val state = Seq(nodes, relations)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}






