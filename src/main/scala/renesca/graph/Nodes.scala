package renesca.graph

import collection.mutable

class Nodes(self: mutable.LinkedHashSet[Node] = mutable.LinkedHashSet.empty[Node])
  extends mutable.Set[Node] with mutable.SetLike[Node, Nodes] {

  private[graph] var graph: Graph = null

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(node: Node) = {
    localChanges += NodeAdd(node)
    self += node
    this
  }

  override def -=(node: Node) = {
    localChanges += NodeDelete(node.id)
    graph.relations.self --= graph.incidentRelations(node)
    self -= node
    this
  }

  override def iterator = self.iterator
  override def contains(node: Node) = self contains node
  override def empty = new Nodes(self.empty)
}
