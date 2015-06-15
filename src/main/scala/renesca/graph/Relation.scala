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
    new Relation(id, start, end, relationType, Create(), properties)
  }
  def create(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty) = {
    apply(Id.nextId(), start, end, relationType, properties)
  }

  def merge(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty) = {
    new Relation(Id.nextId(), start, end, relationType, Merge(merge, onMatch), properties)
  }

  def find(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty) = {
    new Relation(Id.nextId(), start, end, relationType, Match(), properties)
  }
}

class Relation private[Relation](
                                  val id: Id,
                                  val startNode: Node,
                                  val endNode: Node,
                                  val relationType: RelationType,
                                  val origin: ItemOrigin,
                                  initialProperties: PropertyMap = Map.empty
                                  ) extends Item {

  val properties = new Properties(this, mutable.Map(initialProperties.toSeq: _*))

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


