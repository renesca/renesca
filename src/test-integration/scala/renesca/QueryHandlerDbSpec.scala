package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.graph._
import renesca.parameter.{NullPropertyValue, ArrayParameterValue, ParameterValue, PropertyValue}
import renesca.parameter.implicits._
import renesca.table.Table

@RunWith(classOf[JUnitRunner])
class QueryHandlerDbSpec extends IntegrationSpecification {

  def resultNode: Node = {
    val resultGraph = db.queryGraph("match n return n")
    resultGraph.nodes.head
  }

  def resultRelation: Relation = {
    val resultGraph = db.queryGraph("match ()-[r]-() return r")
    resultGraph.relations.head
  }

  def testNodeSetProperty(data: PropertyValue) = {
    val graph = Graph.empty
    val node = Node.created

    graph.nodes += node

    node.properties("key") = data
    db.persistChanges(graph)

    resultNode.properties("key") mustEqual data
  }

  def testRelationSetProperty(data: PropertyValue) = {
    val graph = Graph.empty
    val start = Node.created
    val end = Node.created
    graph.nodes += start
    graph.nodes += end
    val relation = Relation.created(start, "EATS", end)
    graph.relations += relation

    relation.properties("key") = data
    db.persistChanges(graph)

    resultRelation.properties("key") mustEqual data
  }

  "QueryHandler" should {
    "throw exception on Neo4j Error" in {
      db.query("this is invalid cypher syntax") must throwA[RuntimeException]
    }

    "query table" in {
      db.query("create (n {a:1}),(m {a:2})")
      val table = db.queryTable("match x return x.a order by x.a")

      table mustEqual Table(
        columns = List("x.a"),
        data = List(
          List[ParameterValue](1),
          List[ParameterValue](2)
        )
      )
    }

    "return only graphs in json data on queryGraphs" in todo
    "return only parameters in json data on queryTables" in todo
    "return no json data on query" in todo
  }

  "QueryHandler.persist" should {

    "set long property on node" in { testNodeSetProperty(123) }
    "set double property on node" in { testNodeSetProperty(1.337) }
    "set string property on node" in { testNodeSetProperty("schnipp") }
    "set boolean property on node" in { testNodeSetProperty(true) }

    "set long array property on node" in { testNodeSetProperty(List(1, 3)) }
    "set double array property on node" in { testNodeSetProperty(List(1.7, 2.555555)) }
    "set string array property on node" in { testNodeSetProperty(List("schnipp", "schnapp")) }
    "set boolean array property on node" in { testNodeSetProperty(List(true, false)) }

    "remove property from node" in {
      val graph = Graph.empty
      val node = Node.created(properties = Map("yes" -> 0))
      graph.nodes += node
      db.persistChanges(graph)

      node.properties -= "yes"
      db.persistChanges(graph)

      resultNode.properties must beEmpty
    }

    "set label on node" in {
      val graph = Graph.empty
      val node = Node.created
      graph.nodes += node
      db.persistChanges(graph)

      node.labels += Label("BEER")
      db.persistChanges(graph)

      resultNode.labels must contain(exactly(Label("BEER")))
    }

    "remove label from node" in {
      val graph = Graph.empty
      val node = Node.created(Set("WINE"))
      graph.nodes += node
      db.persistChanges(graph)

      node.labels -= Label("WINE")
      db.persistChanges(graph)

      resultNode.labels must beEmpty
    }

    "delete node" in {
      val graph = Graph.empty
      val node = Node.created
      graph.nodes += node
      db.persistChanges(graph)

      graph.nodes -= node
      db.persistChanges(graph)

      val resultGraph = db.queryGraph("match n return n")
      resultGraph.nodes must beEmpty
    }

    "delete node with relations" in {
      // 1. create (m)-r->(n)<-l-(q)
      // 2. query (m)-r->(n)
      // 3. delete n (implicitly deletes relation r from graph and relation l which is only in the database)
      // 4. whole graph should be (m) and (q)

      val graph = Graph.empty
      val m = Node.created
      val n = Node.created
      val o = Node.created
      graph.nodes ++= List(m, n, o)
      val rel1 = Relation.created(m, "INTERNAL", n)
      val rel2 = Relation.created(n, "EXTERNAL", o)
      graph.relations ++= List(rel1, rel2)
      db.persistChanges(graph)

      val reducedGraph = db.queryGraph(Query(
        "match (m)-[r:INTERNAL]->(n) where id(m) = {mid} and id(n) = {nid} return n,r,m",
        Map("mid" -> m.id, "nid" -> n.id)))

      reducedGraph.nodes must haveSize(2) // m, n
      reducedGraph.relations must haveSize(1) // r

      reducedGraph.nodes -= n // deletes node n and relations l,r
      db.persistChanges(reducedGraph)

      val resultGraph = db.queryWholeGraph
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
    "set string array property on relation" in { testRelationSetProperty(List("schnipp", "schnapp")) }
    "set boolean array property on relation" in { testRelationSetProperty(List(true, false)) }

    "remove property from relation" in {
      val graph = Graph.empty
      val start = Node.created
      val end = Node.created
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.created(start, "EATS", end, Map("yes" -> 100))
      graph.relations += relation
      db.persistChanges(graph)

      relation.properties -= "yes"
      db.persistChanges(graph)

      resultRelation.properties must beEmpty
    }

    "delete relation" in {
      val graph = Graph.empty
      val start = Node.created
      val end = Node.created
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.created(start, "EATS", end)
      graph.relations += relation
      db.persistChanges(graph)

      graph.relations -= relation
      db.persistChanges(graph)

      val resultGraph = db.queryWholeGraph
      resultGraph.nodes must haveSize(2)
      resultGraph.relations must beEmpty
    }

    "add node" in {
      val graph = Graph.empty
      val node = Node.created
      graph.nodes += node
      node.id.value must beLessThan(0L)
      db.persistChanges(graph)
      node.id.value must beGreaterThan(0L)

      resultNode.id mustEqual node.id
      resultNode.labels must beEmpty
      resultNode.properties must beEmpty
    }

    "add properties and labels after NodeAdd" in {
      val graph = Graph.empty
      val node = Node.created
      graph.nodes += node
      node.properties += ("test" -> 5)
      node.labels ++= Set("foo", "bar")
      db.persistChanges(graph)

      resultNode.properties mustEqual Map("test" -> 5)
      resultNode.labels must contain(exactly(Label("foo"), Label("bar")))
    }

    "set properties and labels in NodeAdd" in {
      val graph = Graph.empty
      val node = Node.created(Set("foo", "bar"), Map("test" -> 5))
      graph.nodes += node
      db.persistChanges(graph)

      resultNode.properties mustEqual Map("test" -> 5)
      resultNode.labels must contain(exactly(Label("foo"), Label("bar")))
    }

    "add relation" in {
      val graph = Graph.empty
      val start = Node.created(Set("I"))
      val end = Node.created(Set("cheezburger"))
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.created(start, "can haz", end)
      graph.relations += relation
      relation.id.value must beLessThan(0L)
      db.persistChanges(graph)
      relation.id.value must beGreaterThan(0L)

      resultRelation mustEqual relation
      resultRelation.startNode mustEqual start
      resultRelation.endNode mustEqual end
      resultRelation.relationType mustEqual RelationType("can haz")
    }

    "add properties after RelationAdd" in {
      val graph = Graph.empty
      val start = Node.created(Set("I"))
      val end = Node.created(Set("cheezburger"))
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.created(start, "can haz", end)
      graph.relations += relation
      relation.properties += ("one" -> "yes")
      db.persistChanges(graph)

      resultRelation.properties mustEqual Map("one" -> "yes")
    }

    "set properties in RelationAdd" in {
      val graph = Graph.empty
      val start = Node.created
      val end = Node.created
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.created(start, "can haz", end, Map("one" -> "yes"))
      graph.relations += relation
      db.persistChanges(graph)

      resultRelation.properties mustEqual Map("one" -> "yes")
    }
  }

}

