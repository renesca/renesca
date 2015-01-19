package renesca

import renesca.graph._
import renesca.json.PropertyValue._
import renesca.json.ParameterValue._

object GraphManager {
  val graphChangeToQuery:GraphChange => Query = {
    case NodeSetProperty(nodeId, key, value) => Query("match (n) where id(n) = {id} set n += {keyValue}", Map("id" -> nodeId, "keyValue" -> Map(key -> value)))
    case NodeRemoveProperty(nodeId, key) => Query(s"match (n) where id(n) = {id} remove n.`$key`", Map("id" -> nodeId))
    case NodeSetLabel(nodeId, label) => Query(s"match (n) where id(n) = {id} set n:`${label.name}`", Map("id" -> nodeId))
    case NodeRemoveLabel(nodeId, label) => Query(s"match (n) where id(n) = {id} remove n:`${label.name}`", Map("id" -> nodeId))
    case NodeDelete(nodeId) => Query("match (n) where id(n) = {id} optional match (n)-[r]-() delete r,n", Map("id" -> nodeId))
    case RelationSetProperty(relationId, key, value) => Query("match ()-[r]->() where id(r) = {id} set r += {keyValue}", Map("id" -> relationId, "keyValue" -> Map(key -> value)))
    case RelationRemoveProperty(relationId, key) => Query(s"match ()-[r]->() where id(r) = {id} remove r.`$key`", Map("id" -> relationId))
    case RelationDelete(relationId) => Query("match ()-[r]->() where id(r) = {id} delete r", Map("id" -> relationId))
  }
}

class GraphManager {
  import renesca.GraphManager._

  var dbService:DbService = null

  def persistChanges(graph:Graph) {
    val queries:Seq[Query] = graph.changes.map(graphChangeToQuery)
    dbService.batchQuery(queries)

    graph.clearChanges()
  }
}
