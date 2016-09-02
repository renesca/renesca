package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.graph._
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.table.Table

@RunWith(classOf[JUnitRunner])
class QueryHandlerDbSpec extends IntegrationSpecification {

  def resultNode: Node = {
    val resultGraph = db.queryGraph("match (n) return n")
    resultGraph.nodes.head
  }

  def resultRelation: Relation = {
    val resultGraph = db.queryGraph("match ()-[r]-() return r")
    resultGraph.relations.head
  }

  def testNodeSetProperty(data: PropertyValue) = {
    val graph = Graph.empty
    val node = Node.create

    graph.nodes += node

    node.properties("key") = data
    db.persistChanges(graph).isDefined mustEqual false

    resultNode.properties("key") mustEqual data
  }

  def testRelationSetProperty(data: PropertyValue) = {
    val graph = Graph.empty
    val start = Node.create
    val end = Node.create
    graph.nodes += start
    graph.nodes += end
    val relation = Relation.create(start, "EATS", end)
    graph.relations += relation

    relation.properties("key") = data
    db.persistChanges(graph).isDefined mustEqual false

    resultRelation.properties("key") mustEqual data
  }

  "QueryHandler" should {
    "throw exception on Neo4j Error" in {
      db.query("this is invalid cypher syntax") must throwA[RuntimeException]
    }

    "query table" in {
      db.query("create (n {a:1}),(m {a:2})")
      val table = db.queryTable("match (x) return x.a order by x.a")

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
      val node = Node.create(properties = Map("yes" -> 0))
      graph.nodes += node
      db.persistChanges(graph).isDefined mustEqual false

      node.properties -= "yes"
      db.persistChanges(graph).isDefined mustEqual false

      resultNode.properties must beEmpty
    }

    "set label on node" in {
      val graph = Graph.empty
      val node = Node.create
      graph.nodes += node
      db.persistChanges(graph).isDefined mustEqual false

      node.labels += Label("BEER")
      db.persistChanges(graph).isDefined mustEqual false

      resultNode.labels must contain(exactly(Label("BEER")))
    }

    "remove label from node" in {
      val graph = Graph.empty
      val node = Node.create(Set("WINE"))
      graph.nodes += node
      db.persistChanges(graph).isDefined mustEqual false

      node.labels -= Label("WINE")
      db.persistChanges(graph).isDefined mustEqual false

      resultNode.labels must beEmpty
    }

    "delete node" in {
      val graph = Graph.empty
      val node = Node.create
      graph.nodes += node
      db.persistChanges(graph).isDefined mustEqual false

      graph.nodes -= node
      db.persistChanges(graph).isDefined mustEqual false

      val resultGraph = db.queryGraph("match (n) return n")
      resultGraph.nodes must beEmpty
    }

    "delete match node" in {
      val graph = Graph.empty
      val node = Node.create(Set("BEER"))
      val node2 = Node.create(Set("WINE"))
      graph.nodes += node
      graph.nodes += node2
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val del = Node.matches(Set("WINE"))
      graph2.nodes -= del
      db.persistChanges(graph2).isDefined mustEqual false

      val resultGraph = db.queryGraph("match (n) return n")
      resultGraph.nodes.size mustEqual 1
      resultGraph.nodes.head.labels mustEqual Set(Label("BEER"))
    }

    "delete node with relations" in {
      // 1. create (m)-r->(n)<-l-(q)
      // 2. query (m)-r->(n)
      // 3. delete n (implicitly deletes relation r from graph and relation l which is only in the database)
      // 4. whole graph should be (m) and (q)

      val graph = Graph.empty
      val m = Node.create
      val n = Node.create
      val o = Node.create
      graph.nodes ++= List(m, n, o)
      val rel1 = Relation.create(m, "INTERNAL", n)
      val rel2 = Relation.create(n, "EXTERNAL", o)
      graph.relations ++= List(rel1, rel2)
      db.persistChanges(graph).isDefined mustEqual false

      db.queryWholeGraph.nodes must haveSize(3)

      val reducedGraph = db.queryGraph(Query(
        "match (m)-[r:INTERNAL]->(n) where id(m) = {mid} and id(n) = {nid} return n,r,m",
        Map("mid" -> m.origin.asInstanceOf[Id].id, "nid" -> n.origin.asInstanceOf[Id].id)
      ))

      reducedGraph.nodes must haveSize(2) // m, n
      reducedGraph.relations must haveSize(1) // r

      reducedGraph.nodes -= n // deletes node n and relations l,r
      db.persistChanges(reducedGraph).isDefined mustEqual false

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
      val start = Node.create
      val end = Node.create
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.create(start, "EATS", end, Map("yes" -> 100))
      graph.relations += relation
      db.persistChanges(graph).isDefined mustEqual false

      relation.properties -= "yes"
      db.persistChanges(graph).isDefined mustEqual false

      resultRelation.properties must beEmpty
    }

    "delete relation" in {
      val graph = Graph.empty
      val start = Node.create
      val end = Node.create
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.create(start, "EATS", end)
      graph.relations += relation
      db.persistChanges(graph).isDefined mustEqual false

      graph.relations -= relation
      db.persistChanges(graph).isDefined mustEqual false

      val resultGraph = db.queryWholeGraph
      resultGraph.nodes must haveSize(2)
      resultGraph.relations must beEmpty
    }

    "delete match relation" in {
      val graph = Graph.empty
      val start = Node.create
      val end = Node.create
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.create(start, "EATS", end)
      val relation2 = Relation.create(start, "DRINKS", end)
      graph.relations += relation
      graph.relations += relation2
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val relation3 = Relation.matches(start, "EATS", end)
      graph2.relations -= relation3
      db.persistChanges(graph2).isDefined mustEqual false

      val resultGraph = db.queryWholeGraph
      resultGraph.nodes must haveSize(2)
      resultGraph.relations must haveSize(1)
      resultGraph.relations.head.relationType mustEqual RelationType("DRINKS")
    }

    "add node" in {
      val graph = Graph.empty
      val node = Node.create
      graph.nodes += node
      node.origin.kind mustEqual Create.kind
      db.persistChanges(graph).isDefined mustEqual false
      node.origin.kind mustEqual Id.kind

      resultNode.origin mustEqual node.origin
      resultNode.labels must beEmpty
      resultNode.properties must beEmpty
    }

    "add merge node" in {
      def createNode: (Graph, Node) = {
        val graph = Graph.empty
        val node = Node.merge(Seq("merge"), Map("me" -> "be"), Set("me"))
        graph.nodes += node
        (graph, node)
      }

      val (graph, node) = createNode
      node.properties += ("you" -> "not")
      db.persistChanges(graph)

      val (graph2, node2) = createNode
      node2.origin.kind mustEqual Merge.kind
      db.persistChanges(graph2)
      node2.origin.kind mustEqual Id.kind

      node2.origin must beEqualTo(node.origin)
      node2.properties("you") must beEqualTo(node.properties("you"))
    }

    "add merge node with onMatch setter" in {
      val graph = Graph.empty
      val node = Node.create(Seq("merge"))
      graph.nodes += node
      db.persistChanges(graph)

      val graph2 = Graph.empty
      val node2 = Node.merge(Seq("merge"), Map("new" -> "yes"), onMatch = Set("new"))
      graph2.nodes += node2
      db.persistChanges(graph2)

      node2.origin must beEqualTo(node.origin)
      node2.properties("new") must beEqualTo(StringPropertyValue("yes"))
    }

    "add match node" in {
      val graph = Graph.empty
      val node = Node.create(Seq("matsch"), Map("me" -> "be", "you" -> "not"))
      graph.nodes += node
      val labelDistraction = Node.create(Seq("matsch"))
      val propertyDistraction = Node.create(properties = Map("me" -> "be"))
      graph.nodes += labelDistraction
      graph.nodes += propertyDistraction
      db.persistChanges(graph)

      val graph2 = Graph.empty
      val node2 = Node.matches(Seq("matsch"), Map("me" -> "be", "foo" -> "bar"), Set("me"))
      graph2.nodes += node2
      node2.origin.kind mustEqual Match.kind
      db.persistChanges(graph2)
      node2.origin.kind mustEqual Id.kind

      graph2.nodes.size must beEqualTo(1)
      node2.origin must beEqualTo(node.origin)
      node2.properties("you") must beEqualTo(node.properties("you"))
      node2.properties("foo") must beEqualTo(StringPropertyValue("bar"))
    }

    "fail on missing match node results" in {
      val graph = Graph.empty
      val node = Node.matches(Seq("matsch"), Map("me" -> "be"))
      graph.nodes += node
      val failure = db.persistChanges(graph)

      graph.changes.size mustEqual 1
      failure.isDefined mustEqual true
      failure.get startsWith "Query result is missing desired node: "
    }

    "fail on multiple match node results" in {
      val graph = Graph.empty
      graph.nodes += Node.create
      graph.nodes += Node.create
      db.persistChanges(graph).isDefined mustEqual false

      val node = Node.matches
      graph.nodes += node
      val failure = db.persistChanges(graph)

      failure.isDefined mustEqual true
      failure.get startsWith "More than one query result for node: "
    }

    "add properties and labels after NodeAdd" in {
      val graph = Graph.empty
      val node = Node.create
      graph.nodes += node
      node.properties += ("test" -> 5)
      node.labels ++= Set("foo", "bar")
      db.persistChanges(graph).isDefined mustEqual false

      resultNode.properties mustEqual Map("test" -> 5)
      resultNode.labels must contain(exactly(Label("foo"), Label("bar")))
    }

    "set properties and labels in NodeAdd" in {
      val graph = Graph.empty
      val node = Node.create(Set("foo", "bar"), Map("test" -> 5))
      graph.nodes += node
      db.persistChanges(graph).isDefined mustEqual false

      resultNode.properties mustEqual Map("test" -> 5)
      resultNode.labels must contain(exactly(Label("foo"), Label("bar")))
    }

    "add relation" in {
      val graph = Graph.empty
      val start = Node.create(Set("I"))
      val end = Node.create(Set("cheezburger"))
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.create(start, "can haz", end)
      graph.relations += relation
      relation.origin.kind mustEqual Create.kind
      db.persistChanges(graph).isDefined mustEqual false
      relation.origin.kind mustEqual Id.kind

      resultRelation mustEqual relation
      resultRelation.startNode mustEqual start
      resultRelation.endNode mustEqual end
      resultRelation.relationType mustEqual RelationType("can haz")
    }

    "add merge relation" in {
      val (nodeA, nodeB) = (Node.create, Node.create)

      def createRelation: (Graph, Relation) = {
        val graph = Graph.empty
        val relation = Relation.merge(nodeA, "merge", nodeB, Map("me" -> "be"), Set("me"))
        graph.nodes += nodeA
        graph.nodes += nodeB
        graph.relations += relation
        (graph, relation)
      }

      val (graph, relation) = createRelation
      relation.properties += ("you" -> "not")
      db.persistChanges(graph).isDefined mustEqual false

      val (graph2, relation2) = createRelation
      relation2.origin.kind mustEqual Merge.kind
      db.persistChanges(graph2).isDefined mustEqual false
      relation2.origin.kind mustEqual Id.kind

      relation2.origin must beEqualTo(relation.origin)
      relation2.properties("you") must beEqualTo(relation.properties("you"))
    }

    "add merge relation with onMatch setter" in {
      val (nodeA, nodeB) = (Node.create, Node.create)

      def createRelation(graph: Graph, relation: Relation) {
        graph.nodes += nodeA
        graph.nodes += nodeB
        graph.relations += relation
      }

      val graph = Graph.empty
      val relation = Relation.create(nodeA, "merge", nodeB)
      createRelation(graph, relation)
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val relation2 = Relation.merge(nodeA, "merge", nodeB, Map("new" -> "yes"), onMatch = Set("new"))
      createRelation(graph2, relation2)
      db.persistChanges(graph2).isDefined mustEqual false

      relation2.origin must beEqualTo(relation.origin)
      relation2.properties("new") must beEqualTo(StringPropertyValue("yes"))
    }

    "add match relation" in {
      val graph = Graph.empty
      val (nodeA, nodeB) = (Node.create, Node.create)
      val relation = Relation.create(nodeA, "matsch", nodeB, Map("me" -> "be", "you" -> "not"))
      graph.nodes += nodeA
      graph.nodes += nodeB
      graph.relations += relation
      val labelDistraction = Relation.create(nodeA, "matsch", nodeB)
      val propertyDistraction = Relation.create(nodeA, "sand", nodeB, Map("me" -> "be"))
      graph.relations += labelDistraction
      graph.relations += propertyDistraction
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val relation2 = Relation.matches(nodeA, "matsch", nodeB, Map("me" -> "be"), Set("me"))
      graph2.nodes += nodeA
      graph2.nodes += nodeB
      graph2.relations += relation2
      relation2.origin.kind mustEqual Match.kind
      db.persistChanges(graph2).isDefined mustEqual false
      relation2.origin.kind mustEqual Id.kind

      graph2.relations.size must beEqualTo(1)
      relation2.origin must beEqualTo(relation.origin)
      relation2.properties("you") must beEqualTo(relation.properties("you"))
    }

    "fail on missing match relation results" in {
      val graph = Graph.empty
      val (nodeA, nodeB) = (Node.create, Node.create)
      val relation = Relation.matches(nodeA, "matsch", nodeB, Map("me" -> "be"))
      graph.nodes += nodeA
      graph.nodes += nodeB
      graph.relations += relation
      val failure = db.persistChanges(graph)

      failure.isDefined mustEqual true
      failure.get startsWith "Query result is missing desired relation: "
    }

    "fail on multiple match relation results" in {
      val graph = Graph.empty
      val a = Node.create(Seq("a"))
      val b = Node.create(Seq("b"))
      val r = Relation.create(a, "r", b)
      val q = Relation.create(a, "r", b)
      graph.relations ++= Seq(r, q)
      db.persistChanges(graph).isDefined mustEqual false

      val a2 = Node.matches(Seq("a"))
      val b2 = Node.matches(Seq("b"))
      val rq = Relation.matches(a2, "r", b2)
      graph.relations += rq
      val failure = db.persistChanges(graph)

      failure.isDefined mustEqual true
      failure.get startsWith "More than one query result for relation: "
    }

    "add properties after RelationAdd" in {
      val graph = Graph.empty
      val start = Node.create(Set("I"))
      val end = Node.create(Set("cheezburger"))
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.create(start, "can haz", end)
      graph.relations += relation
      relation.properties += ("one" -> "yes")
      db.persistChanges(graph).isDefined mustEqual false

      resultRelation.properties mustEqual Map("one" -> "yes")
    }

    "set properties in RelationAdd" in {
      val graph = Graph.empty
      val start = Node.create
      val end = Node.create
      graph.nodes += start
      graph.nodes += end
      val relation = Relation.create(start, "can haz", end, Map("one" -> "yes"))
      graph.relations += relation
      db.persistChanges(graph).isDefined mustEqual false

      resultRelation.properties mustEqual Map("one" -> "yes")
    }

    "fail on missing match path results" in {
      val graph = Graph.empty
      val (nodeA, nodeB) = (Node.create, Node.create)
      val relation = Relation.matches(nodeA, "matsch", nodeB, Map("me" -> "be"))
      val Right(path) = Path(relation)
      graph += path
      val failure = db.persistChanges(graph)

      failure.isDefined mustEqual true
      failure.get startsWith "Query result is missing desired path: "
    }

    "fail on multiple match path results" in {
      val graph = Graph.empty
      val a = Node.create(Seq("a"))
      val b = Node.create(Seq("b"))
      val r = Relation.create(a, "r", b)
      val Right(pr) = Path(r)
      val q = Relation.create(a, "r", b)
      val Right(pq) = Path(q)
      graph += pr
      graph += pq
      db.persistChanges(graph).isDefined mustEqual false

      val a2 = Node.matches(Seq("a"))
      val b2 = Node.matches(Seq("b"))
      val rq = Relation.matches(a2, "r", b2)
      val Right(prq) = Path(rq)
      graph += prq
      val failure = db.persistChanges(graph)

      failure.isDefined mustEqual true
      failure.get startsWith "More than one query result for path: "
    }

    "add merge Path" in {
      val graph = Graph.empty
      val start = Node.merge(Seq("START"))
      val middle = Node.merge(Seq("MIDDLE"))
      val end = Node.merge(Seq("END"))
      val r1 = Relation.merge(start, "r1", middle)
      val r2 = Relation.merge(middle, "r2", end)
      val Right(path) = Path(r1, r2)
      graph += path
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val start2 = Node.merge(Seq("START"))
      val middle2 = Node.merge(Seq("MIDDLE"))
      val end2 = Node.merge(Seq("END"))
      val r12 = Relation.merge(start2, "r1", middle2)
      val r22 = Relation.merge(middle2, "r2", end2)
      val Right(path2) = Path(r12, r22)
      graph2 += path2
      db.persistChanges(graph2).isDefined mustEqual false

      val wholeGraph = db.queryWholeGraph

      wholeGraph.relations.size mustEqual 2
      wholeGraph.nodes.size mustEqual 3
      start mustEqual start2
      middle mustEqual middle2
      end mustEqual end2
      r1 mustEqual r12
      r2 mustEqual r22
    }

    "add match Path" in {
      val graph = Graph.empty
      val start = Node.create(Seq("START"))
      val middle = Node.create(Seq("MIDDLE"))
      val end = Node.create(Seq("END"))
      val r1 = Relation.create(start, "r1", middle)
      val r2 = Relation.create(middle, "r2", end)
      val Right(path) = Path(r1, r2)
      graph += path
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val start2 = Node.matches(Seq("START"))
      val middle2 = Node.matches(Seq("MIDDLE"))
      val end2 = Node.matches(Seq("END"))
      val r12 = Relation.matches(start2, "r1", middle2)
      val r22 = Relation.matches(middle2, "r2", end2)
      val Right(path2) = Path(r12, r22)
      graph2 += path2
      db.persistChanges(graph2).isDefined mustEqual false

      val wholeGraph = db.queryWholeGraph

      wholeGraph.relations.size mustEqual 2
      wholeGraph.nodes.size mustEqual 3
      start mustEqual start2
      middle mustEqual middle2
      end mustEqual end2
      r1 mustEqual r12
      r2 mustEqual r22
    }

    "add merge Path with matched middle node" in {
      val graph = Graph.empty
      val middle = Node.create(Seq("MIDDLE"))
      graph.nodes += middle
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val start2 = Node.merge(Seq("START"))
      val middle2 = Node.matches(Seq("MIDDLE"))
      val end2 = Node.merge(Seq("END"))
      val r12 = Relation.merge(start2, "r1", middle2)
      val r22 = Relation.merge(middle2, "r2", end2)
      val Right(path2) = Path(r12, r22)
      graph2 += path2
      db.persistChanges(graph2).isDefined mustEqual false

      val wholeGraph = db.queryWholeGraph

      wholeGraph.relations.size mustEqual 2
      wholeGraph.nodes.size mustEqual 3
      middle mustEqual middle2
    }

    "add merge Path with matched start and end node" in {
      val graph = Graph.empty
      val start = Node.create(Seq("START"))
      val end = Node.create(Seq("END"))
      graph.nodes ++= Seq(start, end)
      db.persistChanges(graph).isDefined mustEqual false

      val graph2 = Graph.empty
      val start2 = Node.matches(Seq("START"))
      val middle2 = Node.merge(Seq("MIDDLE"))
      val end2 = Node.matches(Seq("END"))
      val r12 = Relation.merge(start2, "r1", middle2)
      val r22 = Relation.merge(middle2, "r2", end2)
      val Right(path2) = Path(r12, r22)
      graph2 += path2
      db.persistChanges(graph2).isDefined mustEqual false

      val wholeGraph = db.queryWholeGraph

      wholeGraph.relations.size mustEqual 2
      wholeGraph.nodes.size mustEqual 3
      start mustEqual start2
      end mustEqual end2
    }

    "delete match path" in {
      val graph = Graph.empty
      val start = Node.create(Seq("START"))
      val end = Node.create(Seq("END"))
      val middle = Node.create(Seq("MIDDLE"))
      val r1 = Relation.create(start, "r1", middle)
      val r2 = Relation.create(middle, "r2", end)
      val Right(path) = Path(r1, r2)

      val middle2 = Node.create(Seq("MIDDLE2"))
      val r12 = Relation.create(start, "r1", middle2)
      val r22 = Relation.create(middle2, "r2", end)
      val Right(path2) = Path(r12, r22)

      graph += path
      graph += path2
      graph.nodes += middle
      graph.relations += r1
      graph.relations += r2
      db.persistChanges(graph).isDefined mustEqual false

      val matchGraph = Graph.empty
      val matchMiddle = Node.matches(Seq("MIDDLE"))
      val matchr1 = Relation.matches(start, "r1", matchMiddle)
      val matchr2 = Relation.matches(matchMiddle, "r2", end)
      val Right(matchpath) = Path(matchr1, matchr2)

      matchGraph += matchpath
      matchGraph.nodes -= matchMiddle
      matchGraph.relations -= matchr1
      matchGraph.relations -= matchr2
      db.persistChanges(matchGraph).isDefined mustEqual false

      val wholeGraph = db.queryWholeGraph

      wholeGraph.relations.size mustEqual 2
      wholeGraph.nodes.size mustEqual 3
      wholeGraph.nodes.map(_.labels) contains Set(Label("MIDDLE2"))
    }

    "add path with removed middle node" in {
      val graph = Graph.empty
      val start = Node.create(Seq("START"))
      val end = Node.create(Seq("END"))

      val middle = Node.merge(Seq("MIDDLE"))
      val r1 = Relation.merge(start, "r1", middle)
      val r2 = Relation.merge(middle, "r2", end)
      val Right(path) = Path(r1, r2)
      graph += path
      graph.nodes -= middle
      db.persistChanges(graph).isDefined mustEqual false

      val wholeGraph = db.queryWholeGraph

      wholeGraph.relations.size mustEqual 0
      wholeGraph.nodes.size mustEqual 2
    }

    "fail on add path with removed end node" in {
      val graph = Graph.empty
      val start = Node.create(Seq("START"))
      val end = Node.create(Seq("END"))

      val middle = Node.merge(Seq("MIDDLE"))
      val r1 = Relation.merge(start, "r1", middle)
      val r2 = Relation.merge(middle, "r2", end)
      val Right(path) = Path(r1, r2)
      graph += path
      graph.nodes -= end
      val failure = db.persistChanges(graph)

      val wholeGraph = db.queryWholeGraph

      failure.isDefined mustEqual true
      wholeGraph.relations.size mustEqual 0
      wholeGraph.nodes.size mustEqual 0
    }

    "fail on add path with removed relation" in {
      val graph = Graph.empty
      val start = Node.create(Seq("START"))
      val end = Node.create(Seq("END"))

      val middle = Node.merge(Seq("MIDDLE"))
      val r1 = Relation.merge(start, "r1", middle)
      val r2 = Relation.merge(middle, "r2", end)
      val Right(path) = Path(r1, r2)
      graph += path
      graph.relations -= r1
      val failure = db.persistChanges(graph)

      val wholeGraph = db.queryWholeGraph

      failure.isDefined mustEqual true
      wholeGraph.relations.size mustEqual 0
      wholeGraph.nodes.size mustEqual 0
    }

    "duplicate nodes" in {
      val graphA = Graph(Seq(Node.create(Seq("MEH"))))
      db.persistChanges(graphA)

      val graph = Graph(Seq(Node.matches(Seq("MEH")), Node.matches(Seq("MEH"))))
      db.persistChanges(graph)

      graph.nodes.size mustEqual 2
      graph.nodes.head mustEqual graph.nodes.last
      graphA.nodes.size mustEqual 1
      graph.nodes.head mustEqual graphA.nodes.head
    }

    "duplicate nodes with relation" in {
      val graphA = Graph(Seq(Node.create(Seq("MEH"))))
      db.persistChanges(graphA)

      val graph = Graph(relations = Seq(Relation.create(Node.matches(Seq("MEH")), "aha", Node.matches(Seq("MEH")))))
      db.persistChanges(graph)

      graph.relations.size mustEqual 1
      graph.relations.head.startNode mustEqual graph.relations.head.endNode
      graph.nodes.size mustEqual 2
      graph.nodes.head mustEqual graph.nodes.last
      graphA.nodes.size mustEqual 1
      graph.nodes.head mustEqual graphA.nodes.head
    }

    "directly persist nodes" in {
      val a = Node.create(Set("I"))
      val b = Node.merge(Set("cheezburger"))
      db.persistChanges(a, b).isDefined mustEqual false

      a.origin.isLocal mustEqual false
      b.origin.isLocal mustEqual false
    }

    "directly persist relations with nodes" in {
      val start = Node.create(Set("I"))
      val end = Node.create(Set("cheezburger"))
      val relation = Relation.create(start, "can haz", end)
      db.persistChanges(start, relation, end).isDefined mustEqual false

      start.origin.isLocal mustEqual false
      end.origin.isLocal mustEqual false
      relation.origin.isLocal mustEqual false
    }

    "directly persist relations without nodes" in {
      val start = Node.create(Set("I"))
      val end = Node.create(Set("cheezburger"))
      val relation = Relation.create(start, "can haz", end)
      db.persistChanges(relation).isDefined mustEqual false

      start.origin.isLocal mustEqual false
      end.origin.isLocal mustEqual false
      relation.origin.isLocal mustEqual false
    }
    "order by in query returns ordered nodes in graph" in {
      val g = Graph.empty
      for (i <- List(9, 7, 3, 2, 4, 6, 1, 10, 0, 5, 8))
        g.nodes += Node.create(properties = Map("i" -> i))
      db.persistChanges(g)

      val resultGraph = db.queryGraph("match (n) return n order by n.i")
      resultGraph.nodes.map(_.properties("i").asInstanceOf[LongPropertyValue].value).toSeq must contain(exactly(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L).inOrder)
    }

    "order by in query returns ordered nodes in node neighbours" in {
      val g = Graph.empty
      val n = Node.create(List("A"))
      g.nodes += n
      for (i <- List(9, 7, 3, 2, 4, 6, 1, 10, 0, 5, 8))
        g.relations += Relation.create(n, "r", Node.create(properties = Map("i" -> i)))
      db.persistChanges(g)

      implicit val resultGraph = db.queryGraph("match (a:A)-[r]->(n) return a,r,n order by n.i")
      val a = resultGraph.nodes.find(_.labels contains "A").get
      a.neighbours.map(_.properties("i").asInstanceOf[LongPropertyValue].value).toSeq must contain(exactly(0L, 1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L).inOrder)
    }
  }
}
