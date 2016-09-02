package renesca.graph

import renesca.json

import scala.collection.mutable

object Graph {
  def apply(nodes: Traversable[Node] = Nil, relations: Traversable[Relation] = Nil): Graph = {
    val graph = new Graph
    graph.nodes ++= nodes
    graph.relations ++= relations
    graph
  }

  private[renesca] def apply(jsonGraph: json.Graph): Graph = {
    val nodes: List[Node] = jsonGraph.nodes.map { case json.Node(id, labels, properties) =>
      Node(Id(id.toLong), labels.map(Label.apply), properties)
    }

    val idToNode: Map[String, Node] = nodes.flatMap(node => node.origin match {
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

    apply(nodes, relations)
  }

  def empty = apply(Nil, Nil)
}

class Graph private[graph] {
  // private constructor to force usage of Factory

  val nodes = Nodes.empty // TODO: is it possible to eliminate this cyclic reference? (used for removing node incident relations)
  nodes.graph = this
  val relations = Relations.empty // TODO: is it possible to eliminate this cyclic reference? (used for adding nonexistent start/endNode to graph)
  relations.graph = this

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

  private[renesca] def clearChanges() {
    nodes.clearChanges()
    relations.clearChanges()

    localChanges.clear()
  }

  def outRelations(node: Node) = relations.filter(node == _.startNode)
  def inRelations(node: Node) = relations.filter(node == _.endNode)
  def incidentRelations(node: Node) = inRelations(node) ++ outRelations(node)
  def neighbours(node: Node) = incidentRelations(node).map(_.other(node))
  def successors(node: Node) = outRelations(node).map(_.endNode)
  def predecessors(node: Node) = inRelations(node).map(_.startNode)
  def inDegree(node: Node) = inRelations(node).size
  def outDegree(node: Node) = outRelations(node).size
  def degree(node: Node) = inDegree(node) + outDegree(node)

  def merge(that: Graph) = {
    val graph = Graph.apply(this.nodes ++ that.nodes, this.relations ++ that.relations)
    graph.localChanges ++= this.changes
    graph.localChanges ++= that.changes
    graph
  }

  def isEmpty = nodes.isEmpty
  def nonEmpty = nodes.nonEmpty

  override def toString = s"Graph(nodes:(${ nodes.mkString(", ") }), relations:(${ relations.mkString(", ") }))"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Graph]

  override def equals(other: Any): Boolean = other match {
    // we are converting nodes and relations to sets, because order should not matter
    // TODO: is there a more efficient way to do this?
    case that: Graph =>
      (that canEqual this) &&
        this.nodes.toSet == that.nodes.toSet &&
        this.relations.toSet == that.relations.toSet
    case _           => false
  }

  override def hashCode(): Int = {
    // we are converting nodes and relations to sets, because order should not matter
    // TODO: is there a more efficient way to do this?
    val state = Seq(nodes.toSet, relations.toSet)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

