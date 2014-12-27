package renesca.graph

import scala.collection.mutable

object Graph {
  def apply(nodes:Traversable[Node], relations:Traversable[Relation]) = {
    new Graph(
      mutable.HashSet.empty ++ nodes,
      mutable.HashSet.empty ++ relations
    )
  }
}

class Graph private[graph] (val nodes:mutable.Set[Node], val relations:mutable.Set[Relation]) {
  // private constructor to force usage of Factory

  def changes:Seq[GraphChange] = {
    (nodes.flatMap(node => node.changes) ++
    relations.flatMap(relation => relation.changes)).toSeq
  }
}






