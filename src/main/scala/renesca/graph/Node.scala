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

  private[graph] var changes = new mutable.ArrayBuffer[GraphChange]

  // worth a read: https://stackoverflow.com/questions/5827510/how-to-override-apply-in-a-case-class-companion/25538287#25538287

  private[graph] var _labels: NodeLabels = null
  def labels = _labels

  private[graph] var _properties:Properties = null
  def properties = _properties

  def delete(implicit graph:Graph) = {
    graph.nodes -= this
    graph.relations --= this.relations
    changes += NodeDelete(id)
  }

  def outRelations(implicit  graph:Graph) = graph.relations.filter(this == _.start)
  def inRelations(implicit  graph:Graph) = graph.relations.filter(this == _.end)
  def relations(implicit  graph:Graph) = inRelations ++ outRelations
  def neighbours(implicit  graph:Graph) = relations.map(_.other(this))
  def successors(implicit  graph:Graph) = outRelations.map(_.end)
  def predecessors(implicit  graph:Graph) = inRelations.map(_.start)
  def inDegree(implicit  graph:Graph) = inRelations.size
  def outDegree(implicit  graph:Graph) = outRelations.size
  def degree(implicit  graph:Graph) = inDegree + outDegree




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

