package renesca

import renesca.helpers._

trait RelationType

case class Relation(id:Long) { thisRelation =>
  // case class because we want equals and hashcode depending on id
  // TODO: implement equals and hashcode manually to have constructor with start/end

  // when setting graph, update reference in labels and properties
  var _graph: Graph = null
  def graph = _graph
  def graph_=(newGraph:Graph) {
    _graph = newGraph
    properties.graph = newGraph
  }

  var relationType:RelationType = null
  var start: Node = null
  var end: Node = null
  val properties = new Properties {
    override val id = thisRelation.id
    override val setPropertyChange = RelationSetProperty.apply _
    override val removePropertyChange = RelationRemoveProperty.apply _
  }

  def delete() = {
    graph.relations -= this
    graph.changes += RelationDelete(id)
  }

  def other(node:Node) = if(start == node) end else start
}


