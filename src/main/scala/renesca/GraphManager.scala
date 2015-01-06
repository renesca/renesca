package renesca

import renesca.graph._

object GraphManager {
  val graphChangeToQuery:GraphChange => Query = {
    case NodeSetProperty(nodeId, key, value) => Query(s"match (n) where id(n) = {id} set n.$key = {value}", Map("id" -> nodeId, "value" -> value))
    case NodeRemoveProperty(nodeId, key) => ???
    case NodeSetLabel(nodeId, label) => ???
    case NodeRemoveLabel(nodeId, label) => ???
    case NodeDelete(nodeId) => ???
    case RelationSetProperty(relationId, key, value) => ???
    case RelationRemoveProperty(relationId, key) => ???
    case RelationDelete(relationId) => ???
  }
}

class GraphManager {
  import renesca.GraphManager._

  var dbService:DbService = null

  def persistChanges(graph:Graph) {
    val queries:Seq[Query] = graph.changes.map(graphChangeToQuery)
  }
}
