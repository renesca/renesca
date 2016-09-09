package renesca.graph

import renesca.{AbstractDistinctBufferWithFixedType, AbstractDistinctBufferWithFixedTypeFactory, DistinctBufferBuilder}

import scala.collection.mutable

object Nodes extends AbstractDistinctBufferWithFixedTypeFactory[Node, Nodes] {
  override protected[renesca] def constructor(buffer: mutable.ArrayBuffer[Node], set: mutable.HashSet[Node]) = new Nodes(buffer, set)
}

class Nodes private(
                     protected[renesca] val buffer: mutable.ArrayBuffer[Node],
                     protected[renesca] val set: mutable.HashSet[Node])
  extends AbstractDistinctBufferWithFixedType[Node, Nodes] {

  override protected[renesca] def factory = Nodes

  override protected[renesca] def newBuilder =
    new DistinctBufferBuilder(factory.constructor) {
      override def result() = {
        val result = super.result()
        result.graph = graph
        result
      }
    }

  var graph: Graph = null //TODO: move to constructor and factory

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  private[renesca] def clearChanges() = {
    buffer.foreach { node =>
      node.properties.localChanges.clear()
      node.labels.localChanges.clear()
    }

    localChanges.clear()
  }

  override def +=(node: Node) = {
    if(node.origin.isLocal)
      localChanges += AddItem(node)

    super[AbstractDistinctBufferWithFixedType].+=(node)
    this
  }

  //TODO: FIXME override remove
  override def -=(node: Node) = {
    localChanges += DeleteItem(node)

    // delete in and out relations of the to-be-deleted node
    graph.relations --= graph.incidentRelations(node)

    super[AbstractDistinctBufferWithFixedType].-=(node)
    this
  }

  override def clear() = {
    localChanges ++= buffer.map(DeleteItem(_))
    super.clear()
  }

  override def clone() = super.clone().asInstanceOf[Nodes]
}
