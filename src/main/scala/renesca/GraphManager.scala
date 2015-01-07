package renesca

import renesca.graph._
import renesca.json.PropertyValue._
import renesca.json.ParameterValue._

object GraphManager {
  val graphChangeToQuery:GraphChange => Query = {
    case NodeSetProperty(nodeId, key, value) => Query(s"match (n) where id(n) = {id} set {keyvalue}", Map("id" -> nodeId, "keyvalue" -> Map(key -> value)))
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
