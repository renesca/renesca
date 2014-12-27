package renesca.graph

import scala.collection.mutable

object Graph {
  def apply(nodes: Traversable[Node], relations: Traversable[Relation]) = {
    new Graph(
      mutable.HashSet.empty ++ nodes,
      mutable.HashSet.empty ++ relations)
  }
}

class Graph private[graph] (val nodes: mutable.Set[Node], val relations: mutable.Set[Relation]) {
  // private constructor to force usage of Factory
  
  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  def changes: Seq[GraphChange] = {
    localChanges ++
    (nodes.flatMap(node => node.changes) ++
      relations.flatMap(relation => relation.changes)).toSeq
  }

  def delete(relation: Relation) {
    relations -= relation
    localChanges += RelationDelete(relation.id)
  }

  def delete(node: Node) {
    nodes -= node
    relations --= incidentRelations(node)
    localChanges += NodeDelete(node.id)
  }

  def outRelations(node: Node) = relations.filter(node == _.startNode).toSet
  def inRelations(node: Node) = relations.filter(node == _.endNode).toSet
  def incidentRelations(node: Node): Set[Relation] = inRelations(node) ++ outRelations(node)
  def neighbours(node: Node) = incidentRelations(node).map(_.other(node))
  def successors(node: Node) = outRelations(node).map(_.endNode)
  def predecessors(node: Node) = inRelations(node).map(_.startNode)
  def inDegree(node: Node) = inRelations(node).size
  def outDegree(node: Node) = outRelations(node).size
  def degree(node: Node) = inDegree(node) + outDegree(node)

}






