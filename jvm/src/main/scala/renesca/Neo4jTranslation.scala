package renesca

import org.neo4j.driver.v1.Record
import org.neo4j.driver.v1.{types => neo4j}
import renesca.graph._
import renesca.parameter.PropertyKey
import scala.collection.JavaConversions._
import scala.collection.mutable

object Neo4jTranslation {
  def labels(n: neo4j.Node) = n.labels map (s => Label(s))
  def relationType(r: neo4j.Relationship) = RelationType(r.`type`)
  def properties(e: neo4j.Entity) = e.asMap.map { case (k,v) => PropertyKey(k) -> v}.toMap

  def recordsToGraph(records: Seq[Record]): (Graph, Map[Item,String]) = {
    import scala.collection.JavaConversions._

    val variableMapping = mutable.Map.empty[Item, String]

    val nodes = records.flatMap(_.fields collect {
      case Pair(varName: String, n: neo4j.Node) =>
        val node = Node(Id(n.id), labels(n), properties(n))
        variableMapping += node -> varName
        node
    })

    //TODO: more efficient
    val edges = records.flatMap(_.fields collect {
      case Pair(varName: String, r: neo4j.Relationship) =>
        val startNode = nodes.find(_.origin.asInstanceOf[Id].id == r.startNodeId).get
        val endNode = nodes.find(_.origin.asInstanceOf[Id].id == r.endNodeId).get
        val relation = Relation(Id(r.id), startNode, endNode, relationType(r), properties(r))
        variableMapping += relation -> varName
        relation
    })

     (Graph(nodes, edges), variableMapping.toMap)
  }

}
