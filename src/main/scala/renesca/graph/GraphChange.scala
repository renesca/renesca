package renesca.graph

import renesca.parameter.{PropertyKey, PropertyMap, PropertyValue}


sealed trait GraphChange

trait GraphContentChange extends GraphChange {
  val id: Id
}

trait GraphNodeChange extends GraphChange
trait GraphRelationChange extends GraphChange

case class NodeSetProperty(id: Id, key: PropertyKey, value: PropertyValue) extends GraphContentChange with GraphNodeChange

case class NodeRemoveProperty(id: Id, key: PropertyKey) extends GraphContentChange with GraphNodeChange

case class NodeSetLabel(id: Id, label: Label) extends GraphContentChange with GraphNodeChange

case class NodeRemoveLabel(id: Id, label: Label) extends GraphContentChange with GraphNodeChange

case class NodeDelete(id: Id) extends GraphContentChange with GraphNodeChange

case class RelationSetProperty(id: Id, key: PropertyKey, value: PropertyValue) extends GraphContentChange with GraphRelationChange

case class RelationRemoveProperty(id: Id, key: PropertyKey) extends GraphContentChange with GraphRelationChange

case class RelationDelete(id: Id) extends GraphContentChange with GraphRelationChange

trait GraphStructureChange extends GraphChange

case class NodeAdd(node: Node) extends GraphStructureChange with GraphNodeChange

case class RelationAdd(relation: Relation) extends GraphStructureChange with GraphRelationChange
