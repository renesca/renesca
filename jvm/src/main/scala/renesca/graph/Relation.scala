package renesca.graph


import renesca.NonBacktickName
import renesca.parameter.{PropertyKey, PropertyMap}

import scala.collection.mutable

object RelationType {
  implicit def StringToRelationType(name: String): RelationType = RelationType(name)
}

case class RelationType(name: String) extends NonBacktickName

object Relation {
  private[renesca] def apply(id: Id, start: Node, end: Node, relationType: RelationType, properties: PropertyMap = Map.empty) = {
    new Relation(id, start, end, relationType, properties)
  }
  def create(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty) = {
    new Relation(Create(), start, end, relationType, properties)
  }

  def merge(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty) = {
    new Relation(Merge(merge, onMatch), start, end, relationType, properties)
  }

  def matches(start: Node, relationType: RelationType, end: Node, properties: PropertyMap = Map.empty, matches: Set[PropertyKey] = Set.empty) = {
    new Relation(Match(matches), start, end, relationType, properties)
  }
}

class Relation private[Relation](
                                  var origin: Origin,
                                  val startNode: Node,
                                  val endNode: Node,
                                  val relationType: RelationType,
                                  initialProperties: PropertyMap = Map.empty
                                  ) extends Item {

  val properties = new Properties(this, mutable.Map(initialProperties.toSeq: _*))

  def changes: Seq[GraphChange] = properties.localChanges

  def other(node: Node) = if(startNode == node) endNode else startNode

  def canEqual(other: Any): Boolean = other.isInstanceOf[Relation]

  override def equals(other: Any): Boolean = other match {
    case that: Relation => (that canEqual this) && this.origin == that.origin
    case _              => false
  }

  override def hashCode = origin.hashCode

  override def toString = s"$startNode-[${ origin }:$relationType]->$endNode"
}


