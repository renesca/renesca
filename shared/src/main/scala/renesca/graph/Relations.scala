package renesca.graph

import renesca.{AbstractDistinctBufferWithFixedType, AbstractDistinctBufferWithFixedTypeFactory, DistinctBufferBuilder}

import scala.collection.mutable

object Relations extends AbstractDistinctBufferWithFixedTypeFactory[Relation, Relations] {
  override protected[renesca] def constructor(buffer: mutable.ArrayBuffer[Relation], set: mutable.HashSet[Relation]) = new Relations(buffer, set)
}

class Relations private(
                         protected[renesca] val buffer: mutable.ArrayBuffer[Relation],
                         protected[renesca] val set: mutable.HashSet[Relation])
  extends AbstractDistinctBufferWithFixedType[Relation, Relations] {

  override protected[renesca] def factory = Relations

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
    buffer.foreach { relation =>
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

    super[AbstractDistinctBufferWithFixedType].+=(relation)
    this
  }

  //TODO: FIXME override remove
  override def -=(relation: Relation) = {
    localChanges += DeleteItem(relation)
    super[AbstractDistinctBufferWithFixedType].-=(relation)
    this
  }

  override def clear() = {
    localChanges ++= buffer.map(DeleteItem(_))
    super.clear()
  }

  override def clone() = {
    val clone = super.clone().asInstanceOf[Relations]
    clone.graph = graph
    clone
  }
}
