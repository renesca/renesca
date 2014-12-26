package renesca.graph

import renesca.graph.helpers._

import scala.collection.mutable

trait Label

object Node {
  def apply(id:Long, labels:Traversable[Label] = Nil, properties:Map[String,PropertyValue] = Map.empty) = {
    val node = new Node(id)
    node._labels = new NodeLabels(id, mutable.HashSet.empty[Label] ++ labels)
    node._properties = new Properties(id, NodeSetProperty, NodeRemoveProperty,
      mutable.HashMap.empty[String, PropertyValue] ++ properties)
    node
  }
}

class Node private[graph] (val id:Long) { thisNode =>
  // private constructor to force usage of factory

  // worth a read: https://stackoverflow.com/questions/5827510/how-to-override-apply-in-a-case-class-companion/25538287#25538287

  private[graph] var _graph: Graph = null
  def graph = _graph

  private[graph] var _labels: NodeLabels = null
  def labels = _labels

  private[graph] var _properties:Properties = null
  def properties = _properties

  def delete() = {
    graph.nodes -= this
    graph.relations --= this.relations
    graph.changes += NodeDelete(id)
  }

  def outRelations = graph.relations.filter(this == _.start)
  def inRelations = graph.relations.filter(this == _.end)
  def relations = inRelations ++ outRelations
  def neighbours = relations.map(_.other(this))
  def successors = outRelations.map(_.end)
  def predecessors = inRelations.map(_.start)
  def inDegree = inRelations.size
  def outDegree = outRelations.size
  def degree = inDegree + outDegree




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

