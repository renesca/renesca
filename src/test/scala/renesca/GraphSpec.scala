package renesca

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.BeforeExample

class GraphSpec extends Specification with Mockito with BeforeExample {
  sequential

  var A:Node = null
  var B:Node = null
  var ArB:Relation = null
  var nodes:List[Node] = null
  var relations:List[Relation] = null

  override def before {
    A = mock[Node]
    B = mock[Node]
    ArB = mock[Relation]

    nodes = List(A,B)
    relations = List(ArB)
  }

  "Graph" should {
    "create graphs with nodes and relations" in {
      val graph = Graph(nodes, relations)

      graph.nodes must containTheSameElementsAs(nodes)
      graph.relations must containTheSameElementsAs(relations)
    }

    "pass a graph instance to each node and relation" in {
      val graph = Graph(nodes, relations)

      there was one(A).graph_=(graph)
      there was one(B).graph_=(graph)
      there was one(ArB).graph_=(graph)

    }
  }
}
