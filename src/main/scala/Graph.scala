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

object Graph {
  def apply(nodes:Iterable[Node], relations:Iterable[Relation]) = {
    val graph = new Graph(
      mutable.HashSet.empty ++ nodes,
      mutable.HashSet.empty ++ relations
    )

    nodes.foreach{ (n) => n.graph = graph}
    relations.foreach{ (r) => r.graph = graph}

    graph
  }
}

class Graph private (val nodes:mutable.Set[Node], val relations:mutable.Set[Relation]) {

  val changes = new mutable.ArrayBuffer[GraphChange]
}

case class Node(id:Long) { thisNode =>
  // case class because we want equals and hashcode depending on id

  // when setting graph, update reference in labels and properties
  var _graph: Graph = null
  def graph = _graph
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
    graph.nodes -= this
    graph.relations --= this.relations
    graph.changes += NodeDelete(id)
  }

  def outRelations = graph.relations.filter(this == _.start)
  def inRelations = graph.relations.filter(this == _.end)
  def relations = inRelations ++ outRelations
  def neighbours = relations.map(_.other(this))
  def successors = outRelations.map(_.end)
  def predecessors = outRelations.map(_.start)
}

case class Relation(id:Long) { thisRelation =>
  // case class because we want equals and hashcode depending on id
  // TODO: implement equals and hashcode manually to have constructor with start/end

  // when setting graph, update reference in labels and properties
  var _graph: Graph = null
  def graph = _graph
  def graph_=(newGraph:Graph) {
    _graph = newGraph
    properties.graph = newGraph
  }

  var relationType:RelationType = null
  var start: Node = null
  var end: Node = null
  val properties = new Properties {
    override val id = thisRelation.id
    override val setPropertyChange = RelationSetProperty.apply _
    override val removePropertyChange = RelationRemoveProperty.apply _
  }

  def delete() = {
    graph.relations -= this
    graph.changes += RelationDelete(id)
  }

  def other(node:Node) = if(start == node) end else start
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
