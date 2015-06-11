package renesca.graph

import scala.collection.mutable

class NodeLabels(
                  val id: Id,
                  self: mutable.Set[Label] = mutable.HashSet.empty[Label]
                  )
  extends mutable.Set[Label] with mutable.SetLike[Label, NodeLabels] with ChangeableMember {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(label: Label) = {
    addChange(NodeSetLabel(id, label))
    self += label
    this
  }

  override def -=(label: Label) = {
    addChange(NodeRemoveLabel(id, label))
    self -= label
    this
  }

  override def iterator = self.iterator
  override def contains(label: Label) = self contains label
  override def empty = new NodeLabels(id, self.empty)
}
