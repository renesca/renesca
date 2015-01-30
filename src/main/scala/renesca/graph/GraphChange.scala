package renesca.graph

import renesca.json.PropertyValue.PropertyMap
import renesca.json.{PropertyKey, PropertyValue}

sealed trait GraphChange {
  val timestamp:Long = System.nanoTime
}

trait GraphContentChange extends GraphChange
case class NodeSetProperty(nodeId:Id, key:PropertyKey, value:PropertyValue) extends GraphContentChange
case class NodeRemoveProperty(nodeId:Id, key:PropertyKey) extends GraphContentChange
case class NodeSetLabel(nodeId:Id, label:Label) extends GraphContentChange
case class NodeRemoveLabel(nodeId:Id, label:Label) extends GraphContentChange
case class NodeDelete(nodeId:Id) extends GraphContentChange
case class RelationSetProperty(relationId:Id, key:PropertyKey, value:PropertyValue) extends GraphContentChange
case class RelationRemoveProperty(relationId:Id, key:PropertyKey) extends GraphContentChange
case class RelationDelete(relationId:Id) extends GraphContentChange

trait GraphStructureChange extends GraphChange
case class NodeAdd(localNodeId:Id, labels:Traversable[Label] = Nil, properties: PropertyMap = Map.empty) extends GraphStructureChange
case class RelationAdd(relationId: Id, start:Id, end:Id, relationType:RelationType, properties: PropertyMap = Map.empty) extends GraphStructureChange


