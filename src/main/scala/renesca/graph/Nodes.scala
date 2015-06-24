package renesca.graph

import scala.collection.mutable

class Nodes(self: mutable.LinkedHashSet[Node] = mutable.LinkedHashSet.empty[Node])
  extends mutable.Set[Node] with mutable.SetLike[Node, Nodes] {

  private[graph] var graph: Graph = null

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(node: Node) = {
    if(node.origin.isLocal)
      localChanges += AddItem(node)

    self += node
    this
  }

  override def -=(node: Node) = {
    localChanges += DeleteItem(node)
    graph.relations.self --= graph.incidentRelations(node)
    self -= node
    this
  }

  override def iterator = self.iterator
  override def contains(node: Node) = self contains node
  override def empty = new Nodes(self.empty)
}
