package renesca.graph

import scala.collection.mutable

class Nodes(private val graph: Graph, self: mutable.LinkedHashSet[Node] = mutable.LinkedHashSet.empty[Node])
  extends mutable.Set[Node] with mutable.SetLike[Node, Nodes] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  private[renesca] def clearChanges() = {
    self.foreach { node =>
      node.properties.localChanges.clear()
      node.labels.localChanges.clear()
    }

    localChanges.clear()
  }

  override def +=(node: Node) = {
    if(node.origin.isLocal)
      localChanges += AddItem(node)

    self += node
    this
  }

  override def -=(node: Node) = {
    localChanges += DeleteItem(node)

    // delete in and out relations of the to-be-deleted node
    graph.relations --= graph.incidentRelations(node)

    self -= node
    this
  }

  override def iterator = self.iterator
  override def contains(node: Node) = self contains node
  override def empty = new Nodes(graph)
}
