package renesca.graph

import renesca.json.PropertyValue

import scala.collection.mutable

case class RelationType(name:String)

object Relation {
  def apply(id:Long, start:Node, end:Node, relationType:RelationType = null, properties:Map[String,PropertyValue] = Map.empty) = {
    val relationProperties = new Properties(id, RelationSetProperty, RelationRemoveProperty,
        mutable.HashMap.empty[String, PropertyValue] ++ properties)
    new Relation(id, start, end, relationType, relationProperties)
  }
}

class Relation private[Relation] (
    val id:Long,
    val startNode:Node,
    val endNode:Node,
    val relationType:RelationType,
    val properties:Properties
    ) {
  // private constructor to force usage of factory

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]
  def changes:Seq[GraphChange] = localChanges ++ properties.localChanges

  def other(node:Node) = if(startNode == node) endNode else startNode

  def canEqual(other: Any): Boolean = other.isInstanceOf[Relation]

  override def equals(other: Any): Boolean = other match {
    case that: Relation =>
      (that canEqual this) &&
        this.id == that.id
    case _ => false
  }

  override def hashCode = id.hashCode

  override def toString = s"Relation($id: ${startNode.id} -> ${endNode.id})"
}


