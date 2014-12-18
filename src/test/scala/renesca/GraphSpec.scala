package renesca

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope

class GraphSpec extends Specification with Mockito {

  trait ExampleGraph extends Scope {
    val A = mock[Node]
    val B = mock[Node]
    val ArB = mock[Relation]

    val nodes = List(A,B)
    val relations = List(ArB)

    val graph = Graph(nodes, relations)
  }

  "Graph" should {
    "create graphs with nodes and relations" in new ExampleGraph {
      graph.nodes must containTheSameElementsAs(nodes)
      graph.relations must containTheSameElementsAs(relations)
    }

    "pass a graph instance to each node and relation" in new ExampleGraph {
      there was one(A).graph_=(graph)
      there was one(B).graph_=(graph)
      there was one(ArB).graph_=(graph)
    }

  }
}
