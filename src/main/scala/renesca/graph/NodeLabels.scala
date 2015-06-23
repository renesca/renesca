package renesca.graph

import scala.collection.mutable

class NodeLabels(
                  val node: Node,
                  self: mutable.Set[Label] = mutable.HashSet.empty[Label]
                  )
  extends mutable.Set[Label] with mutable.SetLike[Label, NodeLabels] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(label: Label) = {
    if (!node.id.isLocal)
      localChanges += SetLabel(node, label)

    self += label
    this
  }

  override def -=(label: Label) = {
    if (!node.id.isLocal)
      localChanges += RemoveLabel(node, label)

    self -= label
    this
  }

  override def iterator = self.iterator
  override def contains(label: Label) = self contains label
  override def empty = new NodeLabels(node, self.empty)
}
