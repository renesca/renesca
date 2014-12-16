package renesca

import collection.mutable

trait RelationType
trait Label

trait PropertyValue
case class LongPropertyValue(x:Long) extends PropertyValue
case class DoublePropertyValue(x:Double) extends PropertyValue
case class StringPropertyValue(x:String) extends PropertyValue
case class BooleanPropertyValue(x:Boolean) extends PropertyValue
case class ArrayLongPropertyValue(x:Seq[Long]) extends PropertyValue
case class ArrayDoublePropertyValue(x:Seq[Double]) extends PropertyValue
case class ArrayStringPropertyValue(x:Seq[String]) extends PropertyValue
case class ArrayBooleanPropertyValue(x:Seq[Boolean]) extends PropertyValue

object PropertyValue {
  implicit def primitiveToPropertyValue(x:Long):PropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Double):PropertyValue = DoublePropertyValue(x)
  implicit def primitiveToPropertyValue(x:String):PropertyValue = StringPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Boolean):PropertyValue = BooleanPropertyValue(x)

  implicit def SeqLongToPropertyValue(x:Seq[Long]):PropertyValue = ArrayLongPropertyValue(x)
  implicit def SeqDoubleToPropertyValue(x:Seq[Double]):PropertyValue = ArrayDoublePropertyValue(x)
  implicit def SeqStringToPropertyValue(x:Seq[String]):PropertyValue = ArrayStringPropertyValue(x)
  implicit def SeqBooleanToPropertyValue(x:Seq[Boolean]):PropertyValue = ArrayBooleanPropertyValue(x)
}

import PropertyValue._

sealed trait GraphChange
case class NodeSetProperty(nodeId:Long, key:String, value:PropertyValue) extends GraphChange
case class NodeRemoveProperty(nodeId:Long, key:String) extends GraphChange
case class NodeSetLabel(nodeId:Long, label:Label) extends GraphChange
case class NodeRemoveLabel(nodeId:Long, label:Label) extends GraphChange
case class NodeDelete(nodeId:Long) extends GraphChange
case class RelationSetProperty(relationId:Long, key:String, value:PropertyValue) extends GraphChange
case class RelationRemoveProperty(relationId:Long, key:String) extends GraphChange
case class RelationDelete(relationId:Long) extends GraphChange

class GraphManager {
  var dbService:DbService = null

  def persistChanges(graph:Graph) {
    // persist graph.changes
  }
}

class Graph(val nodes:mutable.HashSet[ReadWriteNode], val relations:mutable.HashSet[Relation]) {
  nodes.foreach{_.graph = this}
  relations.foreach{_.graph = this}

  val changes = new mutable.ArrayBuffer[GraphChange]
}

trait Node {
  def id:Long
  def labels:collection.Set[Label]
  def properties:collection.Map[String,PropertyValue]
}

case class ReadOnlyNode(id:Long, labels:Set[Label], properties:Map[String,PropertyValue]) extends Node

case class ReadWriteNode(id:Long) extends Node with GraphRef { thisNode =>
  def graph_=(newGraph:Graph) {
    _graph = newGraph
    labels.graph = newGraph
    properties.graph = newGraph
  }

  val labels = new NodeLabels {
    override val id = thisNode.id
  }

  val properties = new Properties {
    override val id = thisNode.id
    override val setPropertyChange = NodeSetProperty.apply _
    override val removePropertyChange = NodeRemoveProperty.apply _
  }

  def delete() = {
    graph.nodes -= this //TODO: delete relations
    graph.changes += NodeDelete(id)
  }

  def outRelations = graph.relations.filter(this == _.startNode)
  def inRelations = graph.relations.filter(this == _.endNode)
  def relations = inRelations ++ outRelations
  def neighbours = relations.map(_.other(this))
  def successors = outRelations.map(_.endNode)
  def predecessors = outRelations.map(_.startNode)
}

case class Relation(id:Long) extends GraphRef { thisRelation =>
  def graph_=(newGraph:Graph) {
    _graph = newGraph
    properties.graph = newGraph
  }

  var relationType:RelationType = null
  var startNode: Node = null
  var endNode: Node = null
  val properties = new Properties {
    override val id = thisRelation.id
    override val setPropertyChange = RelationSetProperty.apply _
    override val removePropertyChange = RelationRemoveProperty.apply _
  }

  def delete() = {
    graph.relations -= this
    graph.changes += RelationDelete(id)
  }

  def other(node:Node) = if(startNode == node) endNode else startNode
}

trait GraphRef {
  var _graph: Graph = null
  def graph = _graph
  def graph_=(newGraph:Graph)
}

abstract class NodeLabels extends mutable.HashSet[Label] {
  var graph: Graph = null
  val id: Long

  override def +=(elem:Label) = {
    graph.changes += NodeSetLabel(id, elem)
    super.+=(elem)
  }

  override def -=(elem:Label) = {
    graph.changes += NodeRemoveLabel(id, elem)
    super.-=(elem)
  }
}

abstract class Properties extends mutable.HashMap[String, PropertyValue] {
  var graph: Graph = null
  val id: Long
  val setPropertyChange:(Long,String,PropertyValue) => GraphChange
  val removePropertyChange:(Long,String) => GraphChange

  override def +=(keyvalue:(String, PropertyValue)) = {
    graph.changes += setPropertyChange(id, keyvalue._1, keyvalue._2)
    super.+=(keyvalue)
  }

  override def -=(key:String) = {
    graph.changes += removePropertyChange(id, key)
    super.-=(key)
  }
}
