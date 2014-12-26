package renesca.graph

sealed trait GraphChange
case class NodeSetProperty(nodeId:Long, key:String, value:PropertyValue) extends GraphChange
case class NodeRemoveProperty(nodeId:Long, key:String) extends GraphChange
case class NodeSetLabel(nodeId:Long, label:Label) extends GraphChange
case class NodeRemoveLabel(nodeId:Long, label:Label) extends GraphChange
case class NodeDelete(nodeId:Long) extends GraphChange
case class RelationSetProperty(relationId:Long, key:String, value:PropertyValue) extends GraphChange
case class RelationRemoveProperty(relationId:Long, key:String) extends GraphChange
case class RelationDelete(relationId:Long) extends GraphChange


