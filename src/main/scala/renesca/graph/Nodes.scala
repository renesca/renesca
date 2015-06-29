package renesca.graph

import scala.collection.mutable

class Nodes(private val graph: Graph, private[graph] val self: mutable.LinkedHashSet[Node] = mutable.LinkedHashSet.empty[Node])
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

    // look for in and out relations of the to-be-deleted node
    val (localIncidents, nonLocalIncidents) = graph.incidentRelations(node).partition(_.origin.isLocal)
    // local relations should be removed with an according graph change
    graph.relations --= localIncidents
    // id relations should only be removed from the graph without emitting graph changes
    graph.relations.self --= nonLocalIncidents

    self -= node
    this
  }

  override def iterator = self.iterator
  override def contains(node: Node) = self contains node
  override def empty = new Nodes(graph)
}
