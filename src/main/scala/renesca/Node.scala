package renesca

import renesca.helpers._

trait Label

case class Node(id:Long) { thisNode =>
  // case class because we want equals and hashcode depending on id
  // TODO: Factory like in relation

  // when setting graph, update reference in labels and properties
  var _graph: Graph = null
  def graph = _graph
  def graph_=(newGraph:Graph) {
    _graph = newGraph
    labels.graph = newGraph
    properties.graph = newGraph
  }

  val labels = new NodeLabels(id)
  val properties = new Properties(id, NodeSetProperty, NodeRemoveProperty)

  def delete() = {
    graph.nodes -= this
    graph.relations --= this.relations
    graph.changes += NodeDelete(id)
  }

  def outRelations = graph.relations.filter(this == _.start)
  def inRelations = graph.relations.filter(this == _.end)
  def relations = inRelations ++ outRelations
  def neighbours = relations.map(_.other(this))
  def successors = outRelations.map(_.end)
  def predecessors = inRelations.map(_.start)
}

