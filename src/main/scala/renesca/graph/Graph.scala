package renesca.graph

import renesca.json

import scala.collection.mutable

object Graph {
  def apply(nodes: Traversable[Node] = Nil, relations: Traversable[Relation] = Nil):Graph = {
    new Graph(
      mutable.HashSet.empty ++ nodes,
      mutable.HashSet.empty ++ relations)
  }

  def apply(jsonGraph:json.Graph):Graph = {
    val nodes = jsonGraph.nodes.map{ case json.Node(id,labels,properties) =>
      Node(id.toLong, labels.map(Label), properties)
    }
    
    val idToNode = nodes.map{case node => node.id.toString -> node}.toMap

    val relations = jsonGraph.relationships.map{ case json.Relationship(id, relationshipType, startNode, endNode, properties) =>
      Relation(id.toLong,
        idToNode(startNode),
        idToNode(endNode),
        RelationType(relationshipType),
        properties)
    }

    Graph(nodes, relations)
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
    deleteRelations(node)
    localChanges += NodeDelete(node.id)
  }

  def deleteRelations(node : Node) {
    val nodeRelations = incidentRelations(node)
    relations --= nodeRelations
    localChanges ++= nodeRelations.map(relation => RelationDelete(relation.id))
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

  def merge(that:Graph) = Graph(this.nodes ++ that.nodes, this.relations ++ that.relations)
}






