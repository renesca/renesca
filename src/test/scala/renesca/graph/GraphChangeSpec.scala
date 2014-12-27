package renesca.graph

import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.specification.Scope
import scala.collection.mutable
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.graph.helpers.NodeLabels
import renesca.graph.helpers.Properties

@RunWith(classOf[JUnitRunner])
class GraphChangeSpec extends Specification with Mockito {

  "Graph" should {
    "collect all changes in one collection" in {
      val nodeChange = mock[GraphChange]
      val nodeLabelChange = mock[GraphChange]
      val nodePropertiesChange = mock[GraphChange]

      val relationChange = mock[GraphChange]
      val relationPropertiesChange = mock[GraphChange]

      val A = Node(1)
      A.localChanges += nodeChange
      A.labels.localChanges += nodeLabelChange
      A.properties.localChanges += nodePropertiesChange

      val B = Node(2)

      val ArB = Relation(3, A, B)
      ArB.localChanges += relationChange
      ArB.properties.localChanges += relationPropertiesChange

      val graph = Graph(List(A,B), List(ArB))
      
      A.changes must contain(exactly(nodeChange, nodeLabelChange, nodePropertiesChange))
      ArB.changes must contain(exactly(relationChange, relationPropertiesChange))

      graph.changes.size mustEqual 5
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
      val A = new Node(1, mock[NodeLabels], mock[Properties])
    }

    "emit change when setting property" in {
      val properties = new Properties(1, NodeSetProperty , NodeRemoveProperty)
      properties("key") = "value"
      properties += ("key" -> "value")

      properties.localChanges must contain(exactly(
          NodeSetProperty(1, "key", "value").asInstanceOf[GraphChange],
          NodeSetProperty(1, "key", "value").asInstanceOf[GraphChange]
          ))
    }

    "emit change when setting label" in  {
      val labels = new NodeLabels(1)
      val label = mock[Label]

      labels += label

      labels.localChanges must contain(exactly(NodeSetLabel(1, label).asInstanceOf[GraphChange]))
    }
  }
}
