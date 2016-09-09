package renesca.graph

import renesca.parameter.{PropertyKey, PropertyValue}

sealed trait GraphChange {
  def isValid: Boolean
}

sealed trait GraphItemChange extends GraphChange {
  val item: Item
}

sealed trait GraphPathChange extends GraphChange {
  val path: Path

  def isValid = (path.relations ++ path.nodes).forall(_.origin.kind == path.origin.kind)
}

sealed trait GraphContentChange extends GraphItemChange {
  require(isValid, "GraphContentChanges can only be applied to non-local items")

  def isValid = !item.origin.isLocal
}

case class SetProperty(item: Item, key: PropertyKey, value: PropertyValue) extends GraphContentChange

case class RemoveProperty(item: Item, key: PropertyKey) extends GraphContentChange

case class SetLabel(item: Node, label: Label) extends GraphContentChange

case class RemoveLabel(item: Node, label: Label) extends GraphContentChange

//TODO: rename to RemoveItem?
case class DeleteItem(item: Item) extends GraphItemChange {
  def isValid = true
}

sealed trait GraphStructureChange extends GraphChange

case class AddItem(item: Item) extends GraphStructureChange with GraphItemChange {
  require(isValid, "AddItem changes can only be applied to local items")

  def isValid = item.origin.isLocal
}

case class AddPath(path: Path) extends GraphStructureChange with GraphPathChange {
  require(isValid, "AddPath changes can only be applied to local paths")
}
