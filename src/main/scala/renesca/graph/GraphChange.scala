package renesca.graph

import renesca.parameter.{PropertyKey, PropertyMap, PropertyValue}

sealed trait GraphChange

trait GraphContentChange extends GraphChange {
  val item: Item
  require(!item.id.isLocal, "GraphContentChanges can only be applied to non-local items")
}

case class SetProperty(item: Item, key: PropertyKey, value: PropertyValue) extends GraphContentChange

case class RemoveProperty(item: Item, key: PropertyKey) extends GraphContentChange

case class SetLabel(item: Node, label: Label) extends GraphContentChange

case class RemoveLabel(item: Node, label: Label) extends GraphContentChange

case class DeleteItem(item: Item) extends GraphContentChange

trait GraphStructureChange extends GraphChange

case class AddItem(item: Item) extends GraphStructureChange {
  require(item.id.isLocal, "GraphStructureChanges can only be applied to local items")
}

case class AddPath(path: Path) extends GraphStructureChange
