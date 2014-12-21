package renesca

import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection.mutable

class GraphChangeSpec extends Specification with Mockito {

  trait GraphChangesMock extends Scope {
    val graph = mock[Graph]
    graph.changes returns mock[mutable.ArrayBuffer[GraphChange]]
  }

  "Node" should {

    trait NodeGraphChangesMock extends GraphChangesMock with Scope {
      val A = Node(1)
      A.graph = graph
    }

    "emit change when setting property" in new NodeGraphChangesMock {
      A.properties("key") = "value"
      A.properties += ("key" -> "value")

      there were two(graph.changes).+=(NodeSetProperty(1, "key", "value"))
    }

    "emit change when setting label" in new NodeGraphChangesMock {
      val label = mock[Label]

      A.labels += label

      there was one(graph.changes).+=(NodeSetLabel(1, label))
    }
  }
}
