package renesca.graph

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import renesca.graph.helpers.{NodeLabels, Properties}
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphSpec extends Specification with Mockito {

  trait ExampleGraph extends Scope {
    val A = mock[Node]
    A.labels returns mock[NodeLabels]
    A.properties returns mock[Properties]

    val B = mock[Node]
    B.labels returns mock[NodeLabels]
    B.properties returns mock[Properties]

    val ArB = mock[Relation]
    ArB.properties returns mock[Properties]

    val nodes = List(A,B)
    val relations = List(ArB)

    val graph = Graph(nodes, relations)
  }

  "Graph" should {
    "create graphs with nodes and relations" in new ExampleGraph {
      graph.nodes must containTheSameElementsAs(nodes)
      graph.relations must containTheSameElementsAs(relations)
    }
  }
}
