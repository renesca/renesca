package renesca.graph

import scala.collection.mutable

class Relations(private val graph: Graph, private[graph] val self: mutable.LinkedHashSet[Relation] = mutable.LinkedHashSet.empty[Relation])
  extends mutable.Set[Relation] with mutable.SetLike[Relation, Relations] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  def clearChanges() = {
    self.foreach { relation =>
      relation.properties.localChanges.clear()
    }

    localChanges.clear()
  }

  override def +=(relation: Relation) = {
    if(relation.origin.isLocal)
      localChanges += AddItem(relation)

    if(!(graph.nodes contains relation.startNode))
      graph.nodes += relation.startNode
    if(!(graph.nodes contains relation.endNode))
      graph.nodes += relation.endNode

    self += relation
    this
  }

  override def -=(relation: Relation) = {
    localChanges += DeleteItem(relation)
    self -= relation
    this
  }

  override def iterator = self.iterator
  override def contains(relation: Relation) = self contains relation
  override def empty = new Relations(graph)
}
