package renesca.graph

import renesca.json.{PropertyKey, PropertyValue}

sealed trait GraphChange {
  val timestamp:Long = System.nanoTime
}

trait GraphContentChange extends GraphChange
case class NodeSetProperty(nodeId:Long, key:PropertyKey, value:PropertyValue) extends GraphContentChange
case class NodeRemoveProperty(nodeId:Long, key:PropertyKey) extends GraphContentChange
case class NodeSetLabel(nodeId:Long, label:Label) extends GraphContentChange
case class NodeRemoveLabel(nodeId:Long, label:Label) extends GraphContentChange
case class NodeDelete(nodeId:Long) extends GraphContentChange
case class RelationSetProperty(relationId:Long, key:PropertyKey, value:PropertyValue) extends GraphContentChange
case class RelationRemoveProperty(relationId:Long, key:PropertyKey) extends GraphContentChange
case class RelationDelete(relationId:Long) extends GraphContentChange

trait GraphStructureChange extends GraphChange
case class AddNode(localNodeId:Long) extends GraphStructureChange


