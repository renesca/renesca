package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.graph._
import renesca.parameter._
import renesca.parameter.implicits._
import renesca.table.Table

@RunWith(classOf[JUnitRunner])
class QueryBuilderSpec extends Specification with Mockito {
  sequential

  class DeterministicQueryBuilder extends QueryBuilder {
    override def newQueryGenerator = new QueryGenerator {
      var counter = -1
      override def randomVariable = {
        counter += 1
        s"V$counter"
      }
    }
  }

  class FakeQueryBuilder(results: Seq[Seq[(Graph, Table)]]) extends DeterministicQueryBuilder {
    def applyQueries(queryRequests: Seq[() => Seq[QueryConfig]]): Option[String] = {
      var idx = 0
      applyQueries(queryRequests, queries => {
        if (queries.nonEmpty) {
          val res = results(idx)
          idx = idx + 1
          res
        } else {
          Seq.empty
        }
      })
    }
  }

  object FakeQueryBuilder {
    def apply() = new FakeQueryBuilder(Seq.empty)
    def apply(graph: Graph, graphResults: Graph*) = graphs(graph :: graphResults.toList)
    def apply(table: Table, tableResults: Table*) = tables(table :: tableResults.toList)
    def graphs(graph: Seq[Graph], graphResults: Seq[Graph]*) = new FakeQueryBuilder((graph :: graphResults.toList).filter(_.nonEmpty).map(gs => gs.map(g => (g, Table(Seq.empty, Seq.empty)))))
    def tables(table: Seq[Table], tableResults: Seq[Table]*) = new FakeQueryBuilder((table :: tableResults.toList).filter(_.nonEmpty).map(ts => ts.map(t => (Graph.empty, t))))
  }

  def builder = new DeterministicQueryBuilder

  def parameterMap: MapParameterValue = MapParameterValue(Map.empty)

  def parameterMap(keys: String*): ParameterMap = keys.map(k => (PropertyKey(k), parameterMap)).toMap

  def trimQuery(queryStr: String) = queryStr.trim.replaceAll(" +", " ").replace(" )", ")").replace(" ]", "]")

  def q(queryStr: String, parameterMap: ParameterMap) = Query(trimQuery(queryStr), parameterMap)

  def exq(response: Either[String, Seq[() => Seq[QueryConfig]]]): Seq[Seq[Query]] = {
    var counter = 0
    def newId = {
      counter += 1
      Id(counter)
    }

    // to give some kind of error message...better than nosuchelement
    if(response.left.toOption.isDefined)
      throw new Exception("Query unexpectedly failed: " + response.left.get)

    response.right.get.map(getter => {
      val configs = getter()
      configs.map(config => {
        config.item match {
          case i: Item =>
            i.origin = newId
          case p: Path =>
            p.nodes.foreach(_.origin = newId)
            p.relations.foreach(_.origin = newId)
        }
        q(config.query.statement, config.query.parameters)
      })
    }).filter(_.nonEmpty)
  }

  "QueryBuilder" should {
    "have non-constant variables" in {
      val gen = new QueryGenerator
      val a = gen.randomVariable
      val b = gen.randomVariable

      a mustNotEqual b
    }

    "work with empty list of changes" in {
      val queries = exq(builder.generateQueries(Seq.empty))

      queries.size mustEqual 0
    }

    "reject invalid changes" in {
      val node = Node.create
      val changes = Seq(
        AddItem(node)
      )

      node.origin = Id(1)

      val result = builder.generateQueries(changes)

      result.left.toOption.isDefined mustEqual true
      result.left.get startsWith "Found invalid graph change: "
    }

    "node" should {
      "create only once" in {
        val n = Node.create
        val changes = Seq(
          AddItem(n),
          AddItem(n)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "create (V0 {V0_properties}) return V0",
            Map("V0_properties" -> parameterMap))
        ))
      }

      "not delete local node" in {
        val n = Node.create
        val changes = Seq(
          DeleteItem(n)
        )

        val queries = exq(builder.generateQueries(changes))

        queries.size mustEqual 0
      }

      "node deletion after node" in {
        val a = Node.create

        val changes = Seq(
          AddItem(a),
          DeleteItem(a)
        )

        val queries = exq(builder.generateQueries(changes))

        queries.size mustEqual 0
      }

      "node deletion before node" in {
        val a = Node.create

        val changes = Seq(
          DeleteItem(a),
          AddItem(a)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "create (V0 {V0_properties}) return V0",
            Map("V0_properties" -> parameterMap))
        ))
      }

      "create" in {
        val changes = Seq(
          AddItem(Node.create(Seq("v", "f", "l"), Map("18" -> 48)))
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "create (V0 :`v`:`f`:`l` {V0_properties}) return V0",
            Map("V0_properties" -> Map("18" -> 48)))
        ))
      }

      "merge" in {
        val changes = Seq(
          AddItem(Node.merge(Seq("v", "f", "l"), Map("18" -> 48, "B" -> 0, "o" -> 1), Set("18"), Set("B")))
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "merge (V0 :`v`:`f`:`l` {18: {V0_18}}) on create set V0 += {V0_onCreateProperties} on match set V0 += {V0_onMatchProperties} return V0",
            Map("V0_18" -> 48, "V0_onCreateProperties" -> Map("o" -> 1, "B" -> 0), "V0_onMatchProperties" -> Map("B" -> 0)))
        ))
      }

      "match" in {
        val changes = Seq(
          AddItem(Node.matches(Seq("v", "f", "l"), Map("18" -> true, "48" -> true), Set("18")))
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "match (V0 :`v`:`f`:`l` {18: {V0_18}}) set V0 += {V0_properties} return V0",
            Map("V0_18" -> true, "V0_properties" -> Map("48" -> true)))
        ))
      }

      "delete" in {
        val changes = Seq(
          DeleteItem(Node(1))
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "match (V0) where id(V0) = {V0_nodeId} optional match (V0)-[V1]-() delete V1, V0",
            Map("V0_nodeId" -> 1))
        ))
      }

      "create with properties and labels" in {
        val changes = Seq(
          AddItem(Node.create(Seq("labello"), Map("prop" -> "erty")))
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "create (V0 :`labello` {V0_properties})  return V0",
            Map("V0_properties" -> Map("prop" -> "erty")))
        ))
      }

      "set and remove properties and labels" in {
        // create non-local node with positive id
        val node = Node(1)
        val changes = Seq(
          SetProperty(node, "gisela", 3),
          RemoveLabel(node, "helmut"),
          SetLabel(node, "peter"),
          SetProperty(node, "edit", true),
          SetProperty(node, "new", true),
          RemoveProperty(node, "remove"),
          SetProperty(node, "gisela", 3),
          RemoveProperty(node, "new"),
          RemoveLabel(node, "peter"),
          SetLabel(node, "franz")
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q(
            "match (V0) where id(V0) = {V0_itemId} set V0 += {V0_propertyAdditions} remove V0.`remove` remove V0.`new` set V0:`franz` remove V0:`peter` remove V0:`helmut`",
            Map("V0_itemId" -> 1, "V0_propertyAdditions" -> Map("edit" -> true, "gisela" -> 3)))
        ))
      }

      "query result interpretation should fail with no results" in {
        val node = Node.create
        val changes = Seq(
          AddItem(node)
        )

        val q = FakeQueryBuilder(Graph.empty)
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual Some("Query result is missing desired node: (Create())")
        node.origin.isLocal mustEqual true
      }

      "query result interpretation should fail with more than one result" in {
        val a = Node.create
        val b = Node.matches
        val changes = Seq(
          AddItem(a),
          AddItem(b)
        )

        val q = FakeQueryBuilder(Graph(Set(Node(0))), Graph(Set(Node(1), Node(2))))
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual Some("More than one query result for node: (Match(Set()))")
        a.origin.isLocal mustEqual true
        b.origin.isLocal mustEqual true
      }

      "query result interpretation" in {
        val node = Node.matches(matches = Set("z"))
        val changes = Seq(
          AddItem(node)
        )

        val n1 = Node(1, labels = Set("foo"), properties = Map("a" -> 1L))
        val q = new FakeQueryBuilder(Seq(Seq((Graph(Set(n1)), Table(Seq.empty, Seq.empty)))))
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)
        result mustEqual None
        node.origin mustEqual Id(1)
        node.labels must contain(exactly(Label("foo")))
        node.properties("a") mustEqual LongPropertyValue(1L)
        node mustEqual n1
      }
    }

    "Relation" should {
      "fail on delete of relation node" in {
        val n = Node(1)
        val m = Node(2)
        val r = Relation.create(n, "a", m)
        val changes = Seq(
          DeleteItem(n),
          AddItem(r)
        )

        val result = builder.generateQueries(changes)

        result.left.toOption.isDefined mustEqual true
        result.left.get startsWith "Cannot delete start- or endnode of a new relation: "
      }

      "not delete local relation" in {
        val n = Node(1)
        val m = Node(2)
        val r = Relation.create(n, "a", m)
        val changes = Seq(
          DeleteItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries.size mustEqual 0
      }

      "fail on node deletion after relation" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r),
          DeleteItem(a)
        )

        val result = builder.generateQueries(changes)

        result mustEqual Left("Cannot delete start- or endnode of a new relation: (Match(Set()))")
      }

      "node deletion before relation" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)

        val changes = Seq(
          DeleteItem(b),
          AddItem(a),
          AddItem(b),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V4 :`kicks` {V4_properties}]->(V3) return V4",
              Map("V2_nodeId" -> 1, "V4_properties" -> parameterMap, "V3_nodeId" -> 2))
          )
        )
      }

      "relation deletion after relation" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r),
          DeleteItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          )
        )
      }

      "relation deletion before relation" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)

        val changes = Seq(
          DeleteItem(r),
          AddItem(a),
          AddItem(b),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V4 :`kicks` {V4_properties}]->(V3) return V4",
              Map("V2_nodeId" -> 1, "V4_properties" -> parameterMap, "V3_nodeId" -> 2))
          )
        )
      }

      "create" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b, Map("a" -> 1))

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V4 :`kicks` {V4_properties}]->(V3) return V4",
              Map("V2_nodeId" -> 1, "V4_properties" -> Map("a" -> 1), "V3_nodeId" -> 2))
          )
        )
      }

      "create with non local nodes" in {
        val a = Node(1)
        val b = Node(2)
        val c = Node.create
        val r = Relation.create(a, "kicks", b, Map("a" -> 1))

        val changes = Seq(
          AddItem(c),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("create (V0 {V0_properties}) return V0", parameterMap("V0_properties")),
            q("match (V1) where id(V1) = {V1_nodeId} match (V2) where id(V2) = {V2_nodeId} create (V1)-[V3 :`kicks` {V3_properties}]->(V2) return V3",
              Map("V1_nodeId" -> 1, "V3_properties" -> Map("a" -> 1), "V2_nodeId" -> 2))
          )
        )
      }

      "merge" in {
        val a = Node.matches
        val b = Node.merge
        val r = Relation.merge(a, "kicks", b, Map("a" -> 0, "b" -> 1, "c" -> 2), Set("a"), Set("b"))

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("merge (V1) on create set V1 += {V1_onCreateProperties} on match set V1 += {V1_onMatchProperties} return V1",
              parameterMap("V1_onCreateProperties", "V1_onMatchProperties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} merge (V2)-[V4 :`kicks` {a: {V4_a}}]->(V3) on create set V4 += {V4_onCreateProperties} on match set V4 += {V4_onMatchProperties} return V4",
              Map("V2_nodeId" -> 1, "V4_onCreateProperties" -> Map("b" -> 1, "c" -> 2), "V4_onMatchProperties" -> Map("b" -> 1), "V4_a" -> 0, "V3_nodeId" -> 2))
          )
        )
      }

      "match" in {
        val a = Node.matches
        val b = Node.matches
        val r = Relation.matches(a, "kicks", b, Map("a" -> 1), Set("a"))

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("match (V1) return V1", parameterMap)
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} match (V2)-[V4 :`kicks` {a: {V4_a}}]->(V3) return V4",
              Map("V2_nodeId" -> 1, "V4_a" -> 1, "V3_nodeId" -> 2))
          )
        )
      }

      "delete" in {
        val a = Node(1)
        val b = Node(2)
        val r = Relation(3, a, b, "r")

        val changes = Seq(
          DeleteItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q("match ()-[V0]-() where id(V0) = {V0_relationId} delete V0",
            Map("V0_relationId" -> 3))
        ))
      }

      "create with properties" in {
        val a = Node(1)
        val b = Node.create(Seq("the kicked one"), Map("hurt" -> false))
        val r = Relation.create(a, "kicks", b, Map("really?" -> "no!"))

        val changes = Seq(
          AddItem(b),
          AddItem(r)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("create (V0 :`the kicked one` {V0_properties}) return V0",
              Map("V0_properties" -> Map("hurt" -> false)))
          ),
          Seq(
            q("match (V1) where id(V1) = {V1_nodeId} match (V2) where id(V2) = {V2_nodeId} create (V1)-[V3 :`kicks` {V3_properties}]->(V2) return V3",
              Map("V1_nodeId" -> 1, "V3_properties" -> Map("really?" -> "no!"), "V2_nodeId" -> 1))
          )
        )
      }

      "set and remove properties" in {
        val r = Relation(3, Node(1), Node(2), "r")
        val changes = Seq(
          SetProperty(r, "edit", true),
          SetProperty(r, "new", true),
          RemoveProperty(r, "remove"),
          SetProperty(r, "gisela", 3),
          RemoveProperty(r, "new")
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(Seq(
          q("match ()-[V0]->() where id(V0) = {V0_itemId} set V0 += {V0_propertyAdditions} remove V0.`remove` remove V0.`new`",
            Map("V0_itemId" -> 3, "V0_propertyAdditions" -> Map("edit" -> true, "gisela" -> 3)))
        ))
      }

      "query result interpretation should fail with no results" in {
        val a = Node(1)
        val b = Node(2)
        val r = Relation.matches(a, "r", b)
        val changes = Seq(
          AddItem(r)
        )

        val q = FakeQueryBuilder(Graph.empty)
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual Some("Query result is missing desired relation: (1)-[Match(Set()):r]->(2)")
        r.origin.isLocal mustEqual true
      }

      "query result interpretation should fail with more than one result" in {
        val a = Node(1)
        val b = Node.create
        val r = Relation.matches(a, "r", b)
        val changes = Seq(
          AddItem(b),
          AddItem(r)
        )

        val n2 = Node(2)
        val r1 = Relation(3, a, n2, "r")
        val r2 = Relation(4, a, n2, "r")
        val q = FakeQueryBuilder.graphs(Seq(Graph(Set(n2))), Seq(Graph(Set(a, n2), Set(r1, r2))))
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual Some("More than one query result for relation: (1)-[Match(Set()):r]->(Create())")
        b.origin.isLocal mustEqual true
        r.origin.isLocal mustEqual true
      }

      "query result interpretation" in {
        val a = Node(1)
        val b = Node(2)
        val r = Relation.matches(a, "r", b)
        val changes = Seq(
          AddItem(r)
        )

        val r1 = Relation(3, a, b, "r", Map("a" -> 1L))
        val q = FakeQueryBuilder(Graph(Set(a, b), Set(r1)))
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual None
        r.origin mustEqual Id(3)
        r.properties("a") mustEqual LongPropertyValue(1L)
        r mustEqual r1
      }
    }

    "Path" should {
      "fail on same node in paths" in {
        val a = Node(1)
        val a2 = Node(2)
        val b = Node.create
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "back", a)
        val r3 = Relation.create(a2, "kicks", b)
        val r4 = Relation.create(b, "back", a2)
        val Right(p) = Path(r1, r2)
        val Right(p2) = Path(r3, r4)

        val changes = Seq(
          AddPath(p),
          AddPath(p2)
        )

        val result = builder.generateQueries(changes)

        result mustEqual Left("Paths cannot resolve the same nodes")
      }

      "fail on same relation in paths" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "back", a)
        val Right(p) = Path(r)
        val Right(p2) = Path(r, r2)

        val changes = Seq(
          AddPath(p),
          AddPath(p2)
        )

        val result = builder.generateQueries(changes)

        result mustEqual Left("Paths cannot resolve the same relations")
      }

      "fail on circular dependency between paths" in {
        val a = Node.create(Set("a"))
        val b = Node.create(Set("b"))
        val c = Node.matches
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "from", c)
        val Right(p1) = Path(r1, r2)
        val d = Node.merge
        val r3 = Relation.create(b, "kicks", a)
        val r4 = Relation.create(a, "from", d)
        val Right(p2) = Path(r3, r4)

        val changes = Seq(
          AddPath(p1),
          AddPath(p2)
        )

        val result = builder.generateQueries(changes)

        result.left.toOption.isDefined mustEqual true
        result.left.get startsWith "Circular dependency between paths: "
      }

      "overlapping produce paths" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "from", c)
        val Right(p1) = Path(r1, r2)
        val d = Node.create
        val e = Node.create
        val r3 = Relation.create(b, "kicks", d)
        val r4 = Relation.create(d, "to", e)
        val Right(p2) = Path(r3, r4)
        val r5 = Relation.matches(a, "fpp", b)

        val changes = Seq(
          AddItem(r1),
          AddItem(r2),
          AddItem(a),
          AddItem(d),
          AddPath(p2),
          AddItem(b),
          AddItem(e),
          AddItem(r3),
          AddItem(r5),
          AddPath(p1),
          AddItem(r4),
          AddItem(c)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(

          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties")),
            q("merge (V2) on create set V2 += {V2_onCreateProperties} on match set V2 += {V2_onMatchProperties} return V2",
              parameterMap("V2_onCreateProperties", "V2_onMatchProperties"))
          ),
          Seq(
            q("match (V3) where id(V3) = {V3_nodeId} match (V4) where id(V4) = {V4_nodeId} create (V3)-[V6 :`kicks` {V6_properties}]->(V5 {V5_properties}) -[V7 :`from` {V7_properties}]->(V4) return {id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5, labels: labels(V5)} as V5,{id: id(V6), properties: V6} as V6,{id: id(V7), properties: V7} as V7",
              Map("V6_properties" -> parameterMap, "V4_nodeId" -> 3, "V5_properties" -> parameterMap, "V7_properties" -> parameterMap, "V3_nodeId" -> 1))
          ),
          Seq(
            q("match (V8) where id(V8) = {V8_nodeId} match (V9) where id(V9) = {V9_nodeId} create (V8)-[V11 :`kicks` {V11_properties}]->(V10 {V10_properties}) -[V12 :`to` {V12_properties}]->(V9) return {id: id(V8), properties: V8, labels: labels(V8)} as V8,{id: id(V9), properties: V9, labels: labels(V9)} as V9,{id: id(V10), properties: V10, labels: labels(V10)} as V10,{id: id(V11), properties: V11} as V11,{id: id(V12), properties: V12} as V12",
              Map("V8_nodeId" -> 4, "V9_nodeId" -> 2, "V10_properties" -> parameterMap, "V11_properties" -> parameterMap, "V12_properties" -> parameterMap))
          ),
          Seq(
            q("match (V13) where id(V13) = {V13_nodeId} match (V14) where id(V14) = {V14_nodeId} match (V13)-[V15 :`fpp`]->(V14) return V15",
              Map("V13_nodeId" -> 1, "V14_nodeId" -> 4))
          )
        )
      }

      "overlapping produce and relation paths" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.create
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "from", c)
        val Right(p1) = Path(r1, r2)
        val d = Node.create
        val r3 = Relation.create(b, "kicks", c)
        val Right(p2) = Path(r3)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddItem(d),
          AddItem(r3),
          AddPath(p1),
          AddPath(p2)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties")),
            q("create (V2 {V2_properties}) return V2", parameterMap("V2_properties"))
          ),
          Seq(
            q("match (V3) where id(V3) = {V3_nodeId} match (V4) where id(V4) = {V4_nodeId} create (V3)-[V6 :`kicks` {V6_properties}]->(V5 {V5_properties}) -[V7 :`from` {V7_properties}]->(V4) return {id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5, labels: labels(V5)} as V5,{id: id(V6), properties: V6} as V6,{id: id(V7), properties: V7} as V7",
              Map("V6_properties" -> parameterMap, "V4_nodeId" -> 2, "V5_properties" -> parameterMap, "V7_properties" -> parameterMap, "V3_nodeId" -> 1))
          ),
          Seq(
            q("match (V8) where id(V8) = {V8_nodeId} match (V9) where id(V9) = {V9_nodeId} create (V8)-[V10 :`kicks` {V10_properties}]->(V9) return {id: id(V8), properties: V8, labels: labels(V8)} as V8,{id: id(V9), properties: V9, labels: labels(V9)} as V9,{id: id(V10), properties: V10} as V10",
              Map("V8_nodeId" -> 4, "V9_nodeId" -> 2, "V10_properties" -> parameterMap)
            )
          )
        )
      }

      "allow same start/end for paths" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.create
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "from", c)
        val Right(p1) = Path(r1, r2)
        val d = Node.create
        val r3 = Relation.create(a, "kicks", c)
        val Right(p2) = Path(r3)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddItem(d),
          AddItem(r3),
          AddPath(p1),
          AddPath(p2)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties")),
            q("create (V2 {V2_properties}) return V2", parameterMap("V2_properties"))
          ),
          Seq(
            q("match (V3) where id(V3) = {V3_nodeId} match (V4) where id(V4) = {V4_nodeId} create (V3)-[V6 :`kicks` {V6_properties}]->(V5 {V5_properties}) -[V7 :`from` {V7_properties}]->(V4) return {id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5, labels: labels(V5)} as V5,{id: id(V6), properties: V6} as V6,{id: id(V7), properties: V7} as V7",
              Map("V6_properties" -> parameterMap, "V4_nodeId" -> 2, "V5_properties" -> parameterMap, "V7_properties" -> parameterMap, "V3_nodeId" -> 1))
          ),
          Seq(
            q("match (V8) where id(V8) = {V8_nodeId} match (V9) where id(V9) = {V9_nodeId} create (V8)-[V10 :`kicks` {V10_properties}]->(V9) return {id: id(V8), properties: V8, labels: labels(V8)} as V8,{id: id(V9), properties: V9, labels: labels(V9)} as V9,{id: id(V10), properties: V10} as V10",
              Map("V8_nodeId" -> 1, "V9_nodeId" -> 2, "V10_properties" -> parameterMap)
            )
          )
        )
      }

      "fail on node deletion after path" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)
        val Right(p) = Path(r)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r),
          AddPath(p),
          DeleteItem(a)
        )

        val result = builder.generateQueries(changes)

        result mustEqual Left("Cannot delete start- or endnode of a new relation: (Match(Set()))")
      }

      "node deletion before path" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)
        val Right(p) = Path(r)

        val changes = Seq(
          DeleteItem(b),
          AddItem(a),
          AddItem(b),
          AddItem(r),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V4 :`kicks` {V4_properties}]->(V3) return {id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4} as V4",
              Map("V2_nodeId" -> 1, "V3_nodeId" -> 2, "V4_properties" -> parameterMap))
          )
        )
      }

      "do not fail on relation deletion after path" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)
        val Right(p) = Path(r)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r),
          AddPath(p),
          DeleteItem(r)
        )

        val result = builder.generateQueries(changes)

        result.right.toOption.isDefined mustEqual true
      }

      "relation deletion before path" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)
        val Right(p) = Path(r)

        val changes = Seq(
          DeleteItem(r),
          AddItem(a),
          AddItem(b),
          AddItem(r),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V4 :`kicks` {V4_properties}]->(V3) return {id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4} as V4",
              Map("V2_nodeId" -> 1, "V3_nodeId" -> 2, "V4_properties" -> parameterMap))
          )
        )
      }

      "create single relation" in {
        val a = Node.matches
        val b = Node.create
        val r = Relation.create(a, "kicks", b)
        val Right(p) = Path(r)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(r),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V4 :`kicks` {V4_properties}]->(V3) return {id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4} as V4",
              Map("V2_nodeId" -> 1, "V3_nodeId" -> 2, "V4_properties" -> parameterMap))
          )
        )
      }

      "create" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("merge (V1) on create set V1 += {V1_onCreateProperties} on match set V1 += {V1_onMatchProperties} return V1",
              parameterMap("V1_onCreateProperties", "V1_onMatchProperties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V2)-[V5 :`kicks` {V5_properties}]->(V4 {V4_properties}) -[V6 :`from` {V6_properties}]->(V3) return {id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5} as V5,{id: id(V6), properties: V6} as V6",
              Map("V6_properties" -> parameterMap, "V5_properties" -> parameterMap, "V3_nodeId" -> 2, "V2_nodeId" -> 1, "V4_properties" -> parameterMap))
          )
        )
      }

      "create with non local nodes" in {
        val a = Node(1)
        val b = Node(2)
        val c = Node(3)
        val d = Node.create
        val r1 = Relation.create(a, "kicks", b)
        val r2 = Relation.create(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(d),
          AddItem(r1),
          AddItem(r2),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("create (V0 {V0_properties}) return V0", parameterMap("V0_properties")),
            q("match (V1) where id(V1) = {V1_nodeId} match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} create (V1)-[V4 :`kicks` {V4_properties}]->(V2) -[V5 :`from` {V5_properties}]->(V3) return {id: id(V1), properties: V1, labels: labels(V1)} as V1,{id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4} as V4,{id: id(V5), properties: V5} as V5",
              Map("V5_properties" -> parameterMap, "V4_properties" -> parameterMap, "V1_nodeId" -> 1, "V2_nodeId" -> 2, "V3_nodeId" -> 3))
          )
        )
      }

      "merge" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.merge(a, "kicks", b)
        val r2 = Relation.merge(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties")),
            q("merge (V2) on create set V2 += {V2_onCreateProperties} on match set V2 += {V2_onMatchProperties} return V2",
              Map("V2_onCreateProperties" -> parameterMap, "V2_onMatchProperties" -> parameterMap))
          ),
          Seq(
            q("match (V3) where id(V3) = {V3_nodeId} match (V4) where id(V4) = {V4_nodeId} match (V5) where id(V5) = {V5_nodeId} merge (V3)-[V6 :`kicks`]->(V4) -[V7 :`from`]->(V5) on create set V6 += {V6_onCreateProperties} on match set V6 += {V6_onMatchProperties} on create set V7 += {V7_onCreateProperties} on match set V7 += {V7_onMatchProperties} return {id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5, labels: labels(V5)} as V5,{id: id(V6), properties: V6} as V6,{id: id(V7), properties: V7} as V7",
              Map("V5_nodeId" -> 3, "V7_onMatchProperties" -> parameterMap, "V4_nodeId" -> 2, "V6_onCreateProperties" -> parameterMap, "V3_nodeId" -> 1, "V7_onCreateProperties" -> parameterMap, "V6_onMatchProperties" -> parameterMap))
          )
        )
      }

      "merge with merged middle node" in {
        val a = Node.matches
        val b = Node.merge(Seq("MIDDLE"))
        val c = Node.create
        val r1 = Relation.merge(a, "kicks", b)
        val r2 = Relation.merge(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties"))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} merge (V2)-[V5 :`kicks`]->(V4 :`MIDDLE`) -[V6 :`from`]->(V3) on create set V4 += {V4_onCreateProperties} on match set V4 += {V4_onMatchProperties} on create set V5 += {V5_onCreateProperties} on match set V5 += {V5_onMatchProperties} on create set V6 += {V6_onCreateProperties} on match set V6 += {V6_onMatchProperties} return {id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5} as V5,{id: id(V6), properties: V6} as V6",
              Map("V5_onCreateProperties" -> parameterMap, "V4_onCreateProperties" -> parameterMap, "V6_onCreateProperties" -> parameterMap, "V5_onMatchProperties" -> parameterMap, "V3_nodeId" -> 2, "V2_nodeId" -> 1, "V4_onMatchProperties" -> parameterMap, "V6_onMatchProperties" -> parameterMap))
          )
        )
      }

      "match" in {
        val a = Node.matches
        val b = Node.matches
        val c = Node.matches
        val r1 = Relation.matches(a, "kicks", b)
        val r2 = Relation.matches(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("match (V1) return V1", parameterMap)
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} match (V2)-[V5 :`kicks`]->(V4) -[V6 :`from`]->(V3) return {id: id(V2), properties: V2, labels: labels(V2)} as V2,{id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5} as V5,{id: id(V6), properties: V6} as V6",
              Map("V3_nodeId" -> 2, "V2_nodeId" -> 1))
          )
        )
      }

      "match with merge and create node" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.matches(a, "kicks", b)
        val r2 = Relation.matches(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties")),
            q("merge (V2) on create set V2 += {V2_onCreateProperties} on match set V2 += {V2_onMatchProperties} return V2",
              Map("V2_onCreateProperties" -> parameterMap, "V2_onMatchProperties" -> parameterMap))
          ),
          Seq(
            q("match (V3) where id(V3) = {V3_nodeId} match (V4) where id(V4) = {V4_nodeId} match (V5) where id(V5) = {V5_nodeId} match (V3)-[V6 :`kicks`]->(V4) -[V7 :`from`]->(V5) return {id: id(V3), properties: V3, labels: labels(V3)} as V3,{id: id(V4), properties: V4, labels: labels(V4)} as V4,{id: id(V5), properties: V5, labels: labels(V5)} as V5,{id: id(V6), properties: V6} as V6,{id: id(V7), properties: V7} as V7",
              Map("V3_nodeId" -> 1, "V4_nodeId" -> 2, "V5_nodeId" -> 3))
          )
        )
      }

      "remove startnode of path" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.matches(a, "kicks", b)
        val r2 = Relation.matches(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p),
          DeleteItem(a),
          DeleteItem(r1)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("create (V0 {V0_properties}) return V0", parameterMap("V0_properties")),
            q("merge (V1) on create set V1 += {V1_onCreateProperties} on match set V1 += {V1_onMatchProperties} return V1",
              Map("V1_onCreateProperties" -> parameterMap, "V1_onMatchProperties" -> parameterMap))
          ),
          Seq(
            q("match (V2) where id(V2) = {V2_nodeId} match (V3) where id(V3) = {V3_nodeId} match (V2)-[V4 :`from`]->(V3) return V4",
              Map("V2_nodeId" -> 1, "V3_nodeId" -> 2))
          )
        )
      }

      "remove middle node of path" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.matches(a, "kicks", b)
        val r2 = Relation.matches(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p),
          DeleteItem(b),
          DeleteItem(r1),
          DeleteItem(r2)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("merge (V1) on create set V1 += {V1_onCreateProperties} on match set V1 += {V1_onMatchProperties} return V1",
              Map("V1_onCreateProperties" -> parameterMap, "V1_onMatchProperties" -> parameterMap))
          )
        )
      }

      "remove relation of path" in {
        val a = Node.matches
        val b = Node.create
        val c = Node.merge
        val r1 = Relation.matches(a, "kicks", b)
        val r2 = Relation.matches(b, "from", c)
        val Right(p) = Path(r1, r2)

        val changes = Seq(
          AddItem(a),
          AddItem(b),
          AddItem(c),
          AddItem(r1),
          AddItem(r2),
          AddPath(p),
          DeleteItem(r1)
        )

        val queries = exq(builder.generateQueries(changes))

        queries mustEqual Seq(
          Seq(
            q("match (V0) return V0", parameterMap),
            q("create (V1 {V1_properties}) return V1", parameterMap("V1_properties")),
            q("merge (V2) on create set V2 += {V2_onCreateProperties} on match set V2 += {V2_onMatchProperties} return V2",
              Map("V2_onCreateProperties" -> parameterMap, "V2_onMatchProperties" -> parameterMap))
          ),
          Seq(
            q("match (V3) where id(V3) = {V3_nodeId} match (V4) where id(V4) = {V4_nodeId} match (V3)-[V5 :`from`]->(V4) return V5",
              Map("V3_nodeId" -> 2, "V4_nodeId" -> 3))
          )
        )
      }

      "query result interpretation should fail with no results" in {
        val a = Node(1)
        val b = Node(2)
        val r = Relation.matches(a, "r", b)
        val Right(p) = Path(r)
        val changes = Seq(
          AddItem(r),
          AddPath(p)
        )

        val q = FakeQueryBuilder(Graph.empty)
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual Some("Query result is missing desired path: Path((1)-[Match(Set()):r]->(2))")
        r.origin.isLocal mustEqual true
      }

      "query result interpretation should fail with more than one result" in {
        val a = Node(1)
        val b = Node(2)
        val r = Relation.matches(a, "r", b)
        val Right(p) = Path(r)
        val changes = Seq(
          AddItem(r),
          AddPath(p)
        )

        val cols = Seq("V0", "V1", "V2")
        val rows = Seq(
          Seq(MapParameterValue(Map.empty), MapParameterValue(Map.empty), MapParameterValue(Map.empty)),
          Seq(MapParameterValue(Map.empty), MapParameterValue(Map.empty), MapParameterValue(Map.empty))
        )
        val q = FakeQueryBuilder(Table(cols, rows))
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual Some("More than one query result for path: Path((1)-[Match(Set()):r]->(2))")
        r.origin.isLocal mustEqual true
      }

      "query result interpretation" in {
        val a = Node(1)
        val b = Node.matches
        val c = Node(2)
        val r1 = Relation.matches(a, "r", b)
        val r2 = Relation.matches(b, "r", c)
        val Right(p) = Path(r1, r2)
        val changes = Seq(
          AddItem(r1),
          AddItem(b),
          AddItem(r2),
          AddPath(p)
        )

        val cols = Seq("V0", "V1", "V2", "V3", "V4")
        val rows = Seq(
          Seq(
            MapParameterValue(Map("labels" -> ArrayParameterValue(Seq.empty), "properties" -> MapParameterValue(Map.empty), "id" -> 1L)),
            MapParameterValue(Map("labels" -> ArrayParameterValue(Seq.empty), "properties" -> MapParameterValue(Map.empty), "id" -> 2L)),
            MapParameterValue(Map("labels" -> ArrayParameterValue(Seq.empty), "properties" -> MapParameterValue(Map.empty), "id" -> 10L)),
            MapParameterValue(Map("properties" -> MapParameterValue(Map("a" -> 1L)), "id" -> 3L)),
            MapParameterValue(Map("properties" -> MapParameterValue(Map.empty), "id" -> 4L))
          )
        )
        val table = Table(cols, rows)
        val q = FakeQueryBuilder(table)
        val Right(queries) = q.generateQueries(changes)
        val result = q.applyQueries(queries)

        result mustEqual None
        r1.origin mustEqual Id(3L)
        r2.origin mustEqual Id(4L)
        b.origin mustEqual Id(10L)
        r1.properties("a") mustEqual LongPropertyValue(1L)
      }
    }
  }
}
