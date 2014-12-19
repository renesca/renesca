package renesca.helpers

import renesca.{Graph, Label, NodeRemoveLabel, NodeSetLabel}

import scala.collection.mutable

class NodeLabels(val id: Long, self: mutable.Set[Label] = mutable.HashSet.empty[Label])
  extends mutable.Set[Label] with mutable.SetLike[Label, NodeLabels] {

  var graph: Graph = null

  override def +=(elem: Label) = {
    graph.changes += NodeSetLabel(id, elem)
    self += elem
    this
  }

  override def -=(elem: Label) = {
    graph.changes += NodeRemoveLabel(id, elem)
    self += elem
    this
  }

  override def iterator = self.iterator
  override def contains(elem: Label) = self contains elem
  override def empty = new NodeLabels(id, self.empty)
}
