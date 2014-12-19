package renesca

import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection.mutable

class GraphChangeSpec extends Specification with Mockito {

  trait ExampleGraph extends Scope {
    val A = Node(1)
    val graph = Graph(List(A), Nil)
  }

  trait GraphChangesMock extends Scope {
    val graph = mock[Graph].smart
    graph.changes returns mock[mutable.ArrayBuffer[GraphChange]].smart
  }

  "Node" should {

    trait NodeGraphChangesMock extends GraphChangesMock with Scope {
      val A = Node(1)
      A.graph = graph
    }

    "store property" in new ExampleGraph {
      A.properties("key") = "value"

      A.properties("key") mustEqual StringPropertyValue("value")
    }

    "remove property" in new ExampleGraph {
      todo
    }

    "emit change when setting property" in new NodeGraphChangesMock {
      A.properties("key") = "value"

      there was one(graph.changes).+=(NodeSetProperty(1, "key", "value"))
    }

    "store label" in {
      todo
    }

    "remove label" in {
      todo
    }

    "emit change when setting label" in new NodeGraphChangesMock {
      val label = mock[Label]

      A.labels += label

      there was one(graph.changes).+=(NodeSetLabel(1, label))
    }
  }
}
