package renesca.graph

import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection.mutable

class GraphChangeSpec extends Specification with Mockito {

  "Graph" should {
    "collect all changes in one collection" in {
      val nodeChange = mock[GraphChange]
      val nodeLabelChange = mock[GraphChange]
      val nodePropertiesChange = mock[GraphChange]

      val relationChange = mock[GraphChange]
      val relationPropertiesChange = mock[GraphChange]

      val A = Node(1)
      A.changes += nodeChange
      A.labels.changes += nodeLabelChange
      A.properties.changes += nodePropertiesChange

      val B = Node(2)

      val ArB = Relation(3, A, B)
      ArB.changes += relationChange
      ArB.properties.changes += relationPropertiesChange

      val graph = Graph(List(A,B), List(ArB))

      graph.changes must contain(exactly(
        nodeChange,
        nodeLabelChange,
        nodePropertiesChange,
        relationChange,
        relationPropertiesChange
      ))
    }
  }

  "Node" should {

    trait NodeChangesMock extends Scope {
      val A = Node(1)
      A.changes = mock[mutable.ArrayBuffer[GraphChange]]
      A.properties.changes = mock[mutable.ArrayBuffer[GraphChange]]
      A.labels.changes = mock[mutable.ArrayBuffer[GraphChange]]
    }

    "emit change when setting property" in new NodeChangesMock {
      A.properties("key") = "value"
      A.properties += ("key" -> "value")

      there were two(A.properties.changes).+=(NodeSetProperty(1, "key", "value"))
    }

    "emit change when setting label" in new NodeChangesMock {
      val label = mock[Label]

      A.labels += label

      there was one(A.labels.changes).+=(NodeSetLabel(1, label))
    }
  }
}
