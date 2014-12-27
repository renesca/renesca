package renesca.graph

import renesca.graph.helpers._

import scala.collection.mutable

trait Label

object Node {
  def apply(id: Long, labels: Traversable[Label] = Nil, properties: Map[String, PropertyValue] = Map.empty) = {
    val nodeLabels = new NodeLabels(id, mutable.HashSet.empty[Label] ++ labels)
    val nodeProperties = new Properties(id, NodeSetProperty, NodeRemoveProperty, mutable.HashMap.empty[String, PropertyValue] ++ properties)
    new Node(id, nodeLabels, nodeProperties)
  }
}

class Node private[graph] (
    val id: Long,
    val labels: NodeLabels,
    val properties: Properties
    ) {

  val localChanges = mutable.ArrayBuffer.empty[GraphChange]
  def changes:Seq[GraphChange] = localChanges ++ labels.localChanges ++ properties.localChanges

  def delete(implicit graph: Graph) = {
    graph.nodes -= this
    graph.relations --= this.relations
    localChanges += NodeDelete(id)
  }

  def outRelations(implicit graph: Graph) = graph.relations.filter(this == _.startNode)
  def inRelations(implicit graph: Graph) = graph.relations.filter(this == _.endNode)
  def relations(implicit graph: Graph) = inRelations ++ outRelations
  def neighbours(implicit graph: Graph) = relations.map(_.other(this))
  def successors(implicit graph: Graph) = outRelations.map(_.endNode)
  def predecessors(implicit graph: Graph) = inRelations.map(_.startNode)
  def inDegree(implicit graph: Graph) = inRelations.size
  def outDegree(implicit graph: Graph) = outRelations.size
  def degree(implicit graph: Graph) = inDegree + outDegree

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        this.id == that.id
    case _ => false
  }

  override def hashCode: Int = id.hashCode

  override def toString = s"Node($id)"
}

