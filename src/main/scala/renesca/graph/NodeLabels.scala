package renesca.graph

import scala.collection.mutable

class NodeLabels(
    val id: Id,
    self: mutable.Set[Label] = mutable.HashSet.empty[Label]
    )
  extends mutable.Set[Label] with mutable.SetLike[Label, NodeLabels] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(elem: Label) = {
    localChanges += NodeSetLabel(id, elem)
    self += elem
    this
  }

  override def -=(elem: Label) = {
    localChanges += NodeRemoveLabel(id, elem)
    self -= elem
    this
  }

  override def iterator = self.iterator
  override def contains(elem: Label) = self contains elem
  override def empty = new NodeLabels(id, self.empty)
}
