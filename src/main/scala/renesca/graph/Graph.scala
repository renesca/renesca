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

  def changes:Traversable[GraphChange] = {
    nodes.flatMap(node => node.changes ++ node.labels.changes ++ node.properties.changes) ++
    relations.flatMap(relation => relation.changes ++ relation.properties.changes)
  }
}






