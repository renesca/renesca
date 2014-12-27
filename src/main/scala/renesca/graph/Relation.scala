package renesca.graph

import renesca.graph.helpers._

import scala.collection.mutable

trait RelationType

object Relation {
  def apply(id:Long, start:Node, end:Node, relationType:RelationType = null, properties:Map[String,PropertyValue] = Map.empty) = {
    val relation = new Relation(id)
    relation._start = start
    relation._end = end
    relation._relationType = relationType
    relation._properties = new Properties(id, RelationSetProperty, RelationRemoveProperty, mutable.HashMap.empty[String, PropertyValue] ++ properties)
    relation
  }
}

class Relation private[Relation] (val id:Long) { thisRelation =>
  // private constructor to force usage of factory

  private[graph] var changes = new mutable.ArrayBuffer[GraphChange]

  private[graph] var _relationType:RelationType = null
  def relationType = _relationType

  private[graph] var _start: Node = null
  def start = _start

  private[graph] var _end: Node = null
  def end = _end

  private[graph] var _properties:Properties = null
  def properties = _properties

  def delete(implicit graph:Graph) = {
    graph.relations -= this
    changes += RelationDelete(id)
  }

  def other(node:Node) = if(start == node) end else start



  def canEqual(other: Any): Boolean = other.isInstanceOf[Relation]

  override def equals(other: Any): Boolean = other match {
    case that: Relation =>
      (that canEqual this) &&
        this.id == that.id
    case _ => false
  }

  override def hashCode = id.hashCode

  override def toString = s"Relation($id)"
}


