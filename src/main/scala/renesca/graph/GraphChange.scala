package renesca.graph

import renesca.parameter.{PropertyKey, PropertyMap, PropertyValue}
import scala.collection.mutable

sealed trait GraphChange

sealed trait GraphItemChange extends GraphChange {
  val item: Item
}

sealed trait GraphPathChange extends GraphChange {
  val path: Path
}

sealed trait GraphContentChange extends GraphItemChange {
  require(!item.origin.isLocal, "GraphContentChanges can only be applied to non-local items")
}

case class SetProperty(item: Item, key: PropertyKey, value: PropertyValue) extends GraphContentChange

case class RemoveProperty(item: Item, key: PropertyKey) extends GraphContentChange

case class SetLabel(item: Node, label: Label) extends GraphContentChange

case class RemoveLabel(item: Node, label: Label) extends GraphContentChange

case class DeleteItem(item: Item) extends GraphItemChange

sealed trait GraphStructureChange extends GraphChange

case class AddItem(item: Item) extends GraphStructureChange with GraphItemChange {
  require(item.origin.isLocal, "GraphStructureChanges can only be applied to local items")
}

case class AddPath(path: Path) extends GraphStructureChange with GraphPathChange
