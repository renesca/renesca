package renesca

import scala.collection.mutable



object Graph {
  def apply(nodes:Iterable[Node], relations:Iterable[Relation]) = {
    val graph = new Graph(
      mutable.HashSet.empty ++ nodes,
      mutable.HashSet.empty ++ relations
    )

    nodes.foreach{ (n) => n.graph = graph}
    relations.foreach{ (r) => r.graph = graph}

    graph
  }
}

class Graph private (val nodes:mutable.Set[Node], val relations:mutable.Set[Relation]) {
  // private constructor to force usage of Factory

  val changes = new mutable.ArrayBuffer[GraphChange]
}






