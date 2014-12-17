package renesca

import renesca.helpers._

trait RelationType

object Relation {
  def apply(id:Long, start:Node, end:Node) = {
    val relation = new Relation(id)
    relation.start = start
    relation.end = end
    relation
  }
}

case class Relation private (id:Long) { thisRelation =>
  // case class because we want equals and hashcode depending on id

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


