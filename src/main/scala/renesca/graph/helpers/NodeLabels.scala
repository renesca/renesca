package renesca.graph.helpers

import renesca.graph._

import scala.collection.mutable

class NodeLabels(val id: Long, self: mutable.Set[Label] = mutable.HashSet.empty[Label])
  extends mutable.Set[Label] with mutable.SetLike[Label, NodeLabels] {

  private[graph] var changes = new mutable.ArrayBuffer[GraphChange]

  override def +=(elem: Label) = {
    changes += NodeSetLabel(id, elem)
    self += elem
    this
  }

  override def -=(elem: Label) = {
    changes += NodeRemoveLabel(id, elem)
    self -= elem
    this
  }

  override def iterator = self.iterator
  override def contains(elem: Label) = self contains elem
  override def empty = new NodeLabels(id, self.empty)
}
