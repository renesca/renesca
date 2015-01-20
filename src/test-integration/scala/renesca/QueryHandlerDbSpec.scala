package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.graph.{Graph, Label, Node}
import renesca.json.PropertyKey._
import renesca.json.PropertyValue
import renesca.json.PropertyValue._

@RunWith(classOf[JUnitRunner])
class QueryHandlerDbSpec extends IntegrationSpecification {

  def createNode(query:String):(Graph,Node) = {
    val graph = db.queryGraph(query)
    val node = graph.nodes.head
    (graph, node)
  }
  
  def resultNode:Node = {
    val resultGraph = db.queryGraph("match n return n")
    resultGraph.nodes.head
  }

  def testNodeSetProperty(data:PropertyValue) = {
    val (graph,node) = createNode("create n return n")

    node.properties("key") = data
    db.persistChanges(graph)

    resultNode.properties("key") mustEqual data
  }

  def testRelationSetProperty(data:PropertyValue) = {
    val graph = db.queryGraph("create (chicken)-[r:EATS]->(horse) return chicken, r, horse")
    val relation = graph.relations.head

    relation.properties("key") = data
    db.persistChanges(graph)

    val resultRelation = db.queryGraph("match ()-[r]-() return r").relations.head
    resultRelation.properties("key") mustEqual data
  }

  "QueryHandler" should {
    "throw exception on Neo4j Error" in {
      db.batchQuery("this is invalid cypher syntax") must throwA[RuntimeException]
    }
  }

  "QueryHandler.persist" should {

    "set long property on node" in { testNodeSetProperty(123) }
    "set double property on node" in { testNodeSetProperty(1.337) }
    "set string property on node" in { testNodeSetProperty("schnipp") }
    "set boolean property on node" in { testNodeSetProperty(true) }

    "set long array property on node" in { testNodeSetProperty(List(1, 3)) }
    "set double array property on node" in { testNodeSetProperty(List(1.7, 2.555555)) }
    "set string array property on node" in { testNodeSetProperty(List("schnipp","schnapp")) }
    "set boolean array property on node" in { testNodeSetProperty(List(true, false)) }

    "remove property from node" in {
      val (graph,node) = createNode("create n return n")

      node.properties -= "yes"
      db.persistChanges(graph)

      resultNode.properties must beEmpty
    }

    "set label on node" in {
      val (graph,node) = createNode("create n return n")

      node.labels += Label("BEER")
      db.persistChanges(graph)

      resultNode.labels must contain(exactly(Label("BEER")))
    }

    "remove label from node" in {
      val (graph,node) = createNode("create (n:WINE) return n")

      node.labels -= Label("WINE")
      db.persistChanges(graph)

      resultNode.labels must beEmpty
    }

    "delete node" in {
      val (graph,node) = createNode("create n return n")

      graph.delete(node)
      db.persistChanges(graph)

      val resultGraph = db.queryGraph("match n return n")
      resultGraph.nodes must beEmpty
    }

    "delete node with relations" in {
      // 1. create (m)-r->(n)<-l-(q)
      // 2. query (m)-r->(n)
      // 3. delete n (implicitly deletes relation r from graph and relation l which is only in the database)
      // 4. whole graph should be (m) and (q)

      val nid = db.queryGraph("create n return n").nodes.head.id
      val graph = db.queryGraph("match n where id(n) = {id} create (m)-[r:INTERNAL]->(n)<-[l:EXTERNAL]-(q) return n,r,m", Map("id" -> nid))

      graph.nodes must haveSize(2) // m, n
      graph.relations must haveSize(1) // r

      val n = graph.nodes.find(_.id == nid).get
      graph.delete(n) // deletes node n and relations l,r
      db.persistChanges(graph)

      val resultGraph = db.queryGraph("match (n) optional match (n)-[r]-() return n,r")
      resultGraph.nodes must haveSize(2)
      resultGraph.nodes must not contain n
      resultGraph.relations must beEmpty
    }

    "set long property on relation" in { testRelationSetProperty(123) }
    "set double property on relation" in { testRelationSetProperty(1.337) }
    "set string property on relation" in { testRelationSetProperty("schnipp") }
    "set boolean property on relation" in { testRelationSetProperty(true) }

    "set long array property on relation" in { testRelationSetProperty(List(1, 3)) }
    "set double array property on relation" in { testRelationSetProperty(List(1.7, 2.555555)) }
    "set string array property on relation" in { testRelationSetProperty(List("schnipp","schnapp")) }
    "set boolean array property on relation" in { testRelationSetProperty(List(true, false)) }

    "remove property from relation" in {
      val graph = db.queryGraph("create (chicken)-[r:EATS]->(horse) return chicken, r, horse")
      val relation = graph.relations.head

      relation.properties -= "yes"
      db.persistChanges(graph)

      val resultRelation = db.queryGraph("match ()-[r]-() return r").relations.head
      resultRelation.properties must beEmpty
    }

    "delete relation" in {
      val graph = db.queryGraph("create (chicken)-[r:EATS]->(horse) return chicken, r, horse")
      val relation = graph.relations.head

      graph.delete(relation)
      db.persistChanges(graph)

      val resultGraph = db.queryGraph("match (n) optional match (n)-[r]-() return n,r")
      resultGraph.nodes must haveSize(2)
      resultGraph.relations must beEmpty
    }
  }

}

