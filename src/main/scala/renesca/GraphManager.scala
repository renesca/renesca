package renesca

import renesca.graph._

class GraphManager {
  var dbService:DbService = null

  def persistChanges(graph:Graph) {
    val queries:Seq[Query] = graph.changes.map{
      case NodeSetProperty(nodeId, key, value) => ???
      case NodeRemoveProperty(nodeId, key) => ???
      case NodeSetLabel(nodeId, label) => ???
      case NodeRemoveLabel(nodeId, label) => ???
      case NodeDelete(nodeId) => ???
      case RelationSetProperty(relationId, key, value) => ???
      case RelationRemoveProperty(relationId, key) => ???
      case RelationDelete(relationId) => ???
    }
  }
}
