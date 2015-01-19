package renesca.graph

import renesca.NonBacktickName
import renesca.json.PropertyValue
import scala.collection.mutable

case class Label(name:String) extends NonBacktickName

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

  def changes:Seq[GraphChange] = labels.localChanges ++ properties.localChanges

  def outRelations(implicit graph: Graph) = graph.outRelations(this)
  def inRelations(implicit graph: Graph) = graph.inRelations(this)
  def relations(implicit graph: Graph) = graph.incidentRelations(this)
  def neighbours(implicit graph: Graph) = graph.neighbours(this)
  def successors(implicit graph: Graph) = graph.successors(this)
  def predecessors(implicit graph: Graph) = graph.predecessors(this)
  def inDegree(implicit graph: Graph) = graph.inDegree(this)
  def outDegree(implicit graph: Graph) = graph.outDegree(this)
  def degree(implicit graph: Graph) = graph.degree(this)

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

