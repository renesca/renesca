package renesca

import org.neo4j.driver.v1.Record
import org.neo4j.driver.v1.{types => neo4j}
import renesca.graph._
import renesca.parameter.PropertyKey
import scala.collection.JavaConversions._

object Neo4jTranslation {
  def labels(n: neo4j.Node) = n.labels map (s => Label(s))
  def relationType(r: neo4j.Relationship) = RelationType(r.`type`)
  def properties(e: neo4j.Entity) = e.asMap.map { case (k,v) => PropertyKey(k) -> v}.toMap

  def recordsToGraph(records: Seq[Record]): Graph = {
    import scala.collection.JavaConversions._

    val nodes = records.flatMap(_.values collect {
      case n: neo4j.Node =>
        Node(Id(n.id), labels(n), properties(n))
    })

    //TODO: more efficient
    val edges = records.flatMap(_.values collect {
      case r: neo4j.Relationship =>
        val startNode = nodes.find(_.origin.asInstanceOf[Id].id == r.startNodeId).get
        val endNode = nodes.find(_.origin.asInstanceOf[Id].id == r.endNodeId).get
        Relation(Id(r.id), startNode, endNode, relationType(r), properties(r))
    })

     Graph(nodes, edges)
  }

}
