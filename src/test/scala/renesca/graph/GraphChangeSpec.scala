package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.json.StringPropertyValue
import renesca.json.PropertyValue._
import renesca.json.ParameterValue._

@RunWith(classOf[JUnitRunner])
class GraphChangeSpec extends Specification with Mockito {

  "Graph" should {
    "collect all changes in one collection and clear it" in {
      val graphChange = mock[GraphChange]

      val nodeLabelChange = mock[GraphChange]
      val nodePropertiesChange = mock[GraphChange]

      val relationPropertiesChange = mock[GraphChange]

      val A = Node(1)
      A.labels.localChanges += nodeLabelChange
      A.properties.localChanges += nodePropertiesChange

      val B = Node(2)

      val ArB = Relation(3, A, B)
      ArB.properties.localChanges += relationPropertiesChange

      val graph = Graph(List(A,B), List(ArB))
      graph.localChanges += graphChange

      A.changes must contain(exactly(nodeLabelChange, nodePropertiesChange))
      ArB.changes must contain(exactly(relationPropertiesChange))

      graph.changes.size mustEqual 4
      graph.changes must contain(exactly(
        graphChange,
        nodeLabelChange,
        nodePropertiesChange,
        relationPropertiesChange
      ))

      graph.clearChanges()

      graph.changes must beEmpty
    }

    "emit change when deleting node" in {
      val A = Node(1)
      val graph = Graph(List(A), Nil)

      graph.delete(A)

      graph.localChanges must contain(exactly(
        NodeDelete(A.id).asInstanceOf[GraphChange]
      ))
    }

    "emit changes when deleting node with relations" in {
      val A = Node(1)
      val B = Node(2)
      val C = Node(4)
      val ArB = Relation(3, A, B)
      val BrC = Relation(5, B, C)
      val graph = Graph(List(A, B, C), List(ArB, BrC))

      graph.delete(A)

      graph.nodes must not contain A
      graph.relations must not contain ArB
      graph.relations must contain (BrC)
      graph.changes must not contain(RelationDelete(3))
      graph.changes must not contain RelationDelete(5)
    }

    "emit change when deleting relation" in {
      val A = Node(1)
      val B = Node(2)
      val ArB = Relation(3, A, B)
      val graph = Graph(List(A,B), List(ArB))

      graph.delete(ArB)

      graph.changes must contain(exactly(
        RelationDelete(ArB.id).asInstanceOf[GraphChange]
      ))
    }
  }

  "Node" should {
    "emit change when setting property" in {
      val properties = new Properties(1, NodeSetProperty , NodeRemoveProperty)

      properties("key") = "value"
      properties += ("key" -> "value")

      properties.localChanges must contain(exactly(
        NodeSetProperty(1, "key", "value").asInstanceOf[GraphChange],
        NodeSetProperty(1, "key", "value").asInstanceOf[GraphChange]
      ))
    }

    "emit change when removing property" in {
      val properties = new Properties(1, NodeSetProperty , NodeRemoveProperty)

      properties -= "key"

      properties.localChanges must contain(exactly(
          NodeRemoveProperty(1, "key").asInstanceOf[GraphChange]
      ))
    }

    "emit change when setting label" in  {
      val labels = new NodeLabels(1)
      val label = mock[Label]

      labels += label

      labels.localChanges must contain(exactly(
        NodeSetLabel(1, label).asInstanceOf[GraphChange]
      ))
    }

    "emit change when removing label" in  {
      val labels = new NodeLabels(1)
      val label = mock[Label]

      labels -= label

      labels.localChanges must contain(exactly(
        NodeRemoveLabel(1, label).asInstanceOf[GraphChange]
      ))
    }
  }

  "Relation" should {
    "emit change when setting property" in {
      val relation = Relation(1, Node(2), Node(3))
      relation.properties += (("key", StringPropertyValue("value")))
      relation.properties -= "key"

      relation.changes must contain(
        RelationSetProperty(1, "key", StringPropertyValue("value"))
      )
    }

    "emit change when removing property" in {
      val relation = Relation(1, Node(2), Node(3))
      relation.properties += (("key", StringPropertyValue("value")))
      relation.properties -= "key"

      relation.changes must contain(
          RelationSetProperty(1, "key", StringPropertyValue("value")).asInstanceOf[GraphChange],
          RelationRemoveProperty(1, "key").asInstanceOf[GraphChange]
      ).inOrder
    }
  }
}
