package renesca.graph


import renesca.NonBacktickName
import renesca.parameter.{PropertyKey, PropertyMap, PropertyValue}

import scala.collection.mutable

object RelationType {
  implicit def StringToRelationType(name: String): RelationType = RelationType(name)
}

case class RelationType(name: String) extends NonBacktickName

object Relation {
  private[renesca] def apply(id: Id, start: Node, end: Node, relationType: RelationType, properties: PropertyMap = Map.empty) = {
    val relationProperties = new Properties(id, RelationSetProperty, RelationRemoveProperty, mutable.HashMap.empty[PropertyKey, PropertyValue] ++ properties)
    new Relation(id, start, end, relationType, relationProperties)
  }
  def create(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty) = {
    val relation = apply(Id.nextId(), start, end, relationType)
    relation.properties ++= properties
    relation
  }
}

class Relation private[Relation](
                                  val id: Id,
                                  val startNode: Node,
                                  val endNode: Node,
                                  val relationType: RelationType,
                                  val properties: Properties
                                  ) {
  // private constructor to force usage of factory

  def changes: Seq[GraphChange] = properties.localChanges

  def other(node: Node) = if(startNode == node) endNode else startNode

  def canEqual(other: Any): Boolean = other.isInstanceOf[Relation]

  override def equals(other: Any): Boolean = other match {
    case that: Relation =>
      (that canEqual this) &&
        this.id == that.id
    case _              => false
  }

  override def hashCode = id.hashCode

  override def toString = s"$startNode-[$id:$relationType]->$endNode"
}


