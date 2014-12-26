package renesca.graph

import scala.collection.mutable

object Graph {
  def apply(nodes:Traversable[Node], relations:Traversable[Relation]) = {
    val graph = new Graph(
      mutable.HashSet.empty ++ nodes,
      mutable.HashSet.empty ++ relations
    )

    // TODO: is it possible to avoid the circular references?
    nodes.foreach{ node =>
      node._graph = graph
      node.labels._graph = graph
      node.properties._graph = graph
    }

    relations.foreach{ relation =>
      relation._graph = graph
      relation.properties._graph = graph
    }

    graph
  }
}

class Graph private[graph] (val nodes:mutable.Set[Node], val relations:mutable.Set[Relation]) {
  // private constructor to force usage of Factory

  val changes = new mutable.ArrayBuffer[GraphChange]
}






