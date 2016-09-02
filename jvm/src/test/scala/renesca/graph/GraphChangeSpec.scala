package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._

@RunWith(classOf[JUnitRunner])
class GraphChangeSpec extends Specification with Mockito {

  "GraphChange" should {
    "fail when emitting AddItem/AddPath with non local item" in {
      val node = Node(1)
      AddItem(node) must throwA[IllegalArgumentException]
      val relation = Relation.create(node, "r", node)
      val Right(path) = Path(relation)
      relation.origin = Id(1)
      AddPath(path) must throwA[IllegalArgumentException]
    }

    "fail when emitting ContentChange with local item" in {
      val node = Node.create
      DeleteItem(node).isValid mustEqual true
      DeleteItem(Node(1)).isValid mustEqual true
      SetProperty(node, "a", 1) must throwA[IllegalArgumentException]
      RemoveProperty(node, "a") must throwA[IllegalArgumentException]
      SetLabel(node, "a") must throwA[IllegalArgumentException]
      RemoveLabel(node, "a") must throwA[IllegalArgumentException]
    }

    "allow emitting DeleteItem with local item" in {
      DeleteItem(Node.create).isValid mustEqual true
      DeleteItem(Node(1)).isValid mustEqual true
    }
  }

  "Graph" should {
    "collect all changes in one collection and clear it" in {
      val graphChange = mock[GraphChange]

      val nodesChange = mock[GraphChange]
      val relationsChange = mock[GraphChange]

      val nodeLabelChange = mock[GraphChange]
      val nodePropertiesChange = mock[GraphChange]

      val relationPropertiesChange = mock[GraphChange]

      val A = Node(1)
      A.labels.localChanges += nodeLabelChange
      A.properties.localChanges += nodePropertiesChange

      val B = Node(2)

      val ArB = Relation(3, A, B, "r")
      ArB.properties.localChanges += relationPropertiesChange

      val graph = Graph(List(A, B), List(ArB))
      graph.localChanges += graphChange
      graph.nodes.localChanges += nodesChange
      graph.relations.localChanges += relationsChange

      A.changes must contain(exactly(nodeLabelChange, nodePropertiesChange))
      ArB.changes must contain(exactly(relationPropertiesChange))

      graph.changes.size mustEqual 6
      graph.changes must contain(exactly(
        graphChange,
        nodesChange,
        relationsChange,
        nodeLabelChange,
        nodePropertiesChange,
        relationPropertiesChange
      ))

      graph.clearChanges()

      graph.changes must beEmpty
    }

    "emit change when adding node to nodes" in {
      val graph = Graph.empty
      val node = Node.create

      graph.nodes += node

      graph.nodes.localChanges must contain(exactly(
        AddItem(node).asInstanceOf[GraphChange]
      ))
    }

    "emit changes for nonexistent nodes when adding relation" in {
      val graph = Graph.empty
      val start = Node.create
      val end = Node.create
      val relation = Relation.create(start, "r", end)

      graph.relations += relation

      graph.nodes.localChanges must contain(exactly(
        AddItem(start).asInstanceOf[GraphChange],
        AddItem(end).asInstanceOf[GraphChange]
      ))
      graph.relations.localChanges must contain(exactly(
        AddItem(relation).asInstanceOf[GraphChange]
      ))
    }

    "emit change when deleting node from nodes" in {
      val A = Node(1)
      val graph = Graph(List(A), Nil)

      graph.nodes -= A

      graph.nodes.localChanges must contain(exactly(
        DeleteItem(A).asInstanceOf[GraphChange]
      ))
    }

    "emit change when clearing nodes" in {
      val A = Node(1)
      val graph = Graph(List(A), Nil)

      graph.nodes.clear()

      graph.nodes must beEmpty

      graph.nodes.localChanges must contain(exactly(
        DeleteItem(A).asInstanceOf[GraphChange]
      ))
    }

    "emit changes when deleting node with relations from nodes" in {
      val A = Node(1)
      val B = Node(2)
      val C = Node(4)
      val ArB = Relation(3, A, B, "r")
      val BrC = Relation(5, B, C, "r")
      val graph = Graph(List(A, B, C), List(ArB, BrC))

      graph.nodes -= A

      graph.nodes must not contain A
      graph.relations must not contain ArB
      graph.relations must contain(BrC)
      graph.changes must not contain DeleteItem(BrC)
    }

    "emit change when deleting relation" in {
      val A = Node(1)
      val B = Node(2)
      val ArB = Relation(3, A, B, "r")
      val graph = Graph(List(A, B), List(ArB))

      graph.relations -= ArB

      graph.changes must contain(exactly(
        DeleteItem(ArB).asInstanceOf[GraphChange]
      ))
    }

    "emit change when clearing relations" in {
      val A = Node(1)
      val B = Node(2)
      val ArB = Relation(3, A, B, "r")
      val graph = Graph(List(A, B), List(ArB))

      graph.relations.clear()

      graph.relations must beEmpty

      graph.changes must contain(exactly(
        DeleteItem(ArB).asInstanceOf[GraphChange]
      ))
    }

    "emit change when adding local node with properties/labels" in {
      val graph = Graph.empty
      val node = Node.create

      node.properties("ciao") = "mit V"
      node.labels += "boom"
      graph.nodes += node

      graph.changes must contain(exactly(
        AddItem(node).asInstanceOf[GraphChange]
      )).inOrder
    }

    "emit change when adding local node and then properties/labels" in {
      val graph = Graph.empty
      val node = Node.create

      graph.nodes += node
      node.properties("ciao") = "mit V"
      node.labels += "boom"

      graph.changes must contain(exactly(
        AddItem(node).asInstanceOf[GraphChange]
      )).inOrder
    }

    "emit change when adding local relation with properties" in {
      val node1 = Node(1)
      val node2 = Node(2)
      val graph = Graph(List(node1, node2))
      val relation = Relation.create(node1, "nagut", node2)

      relation.properties("ciao") = "mit V"
      graph.relations += relation

      graph.changes must contain(exactly(
        AddItem(relation).asInstanceOf[GraphChange]
      )).inOrder
    }

    "emit change when adding local relation and then properties" in {
      val node1 = Node(1)
      val node2 = Node(2)
      val graph = Graph(List(node1, node2))
      val relation = Relation.create(node1, "nagut", node2)

      graph.relations += relation
      relation.properties("ciao") = "mit V"

      graph.changes must contain(exactly(
        AddItem(relation).asInstanceOf[GraphChange]
      )).inOrder
    }

    "emit change when using Graph apply for local nodes/relations" in {
      val node1 = Node(1)
      val node2 = Node.create
      val relation = Relation.create(node1, "nagut", node2)
      val graph = Graph(List(node1, node2), List(relation))

      graph.changes must contain(exactly(
        AddItem(node2).asInstanceOf[GraphChange],
        AddItem(relation).asInstanceOf[GraphChange]
      )).inOrder
    }

    "emit change when adding a path" in {
      val node1 = Node(1)
      val node2 = Node(2)
      val graph = Graph(List(node1, node2))
      val relation = Relation.create(node1, "nagut", node2)
      val Right(path) = Path(relation)

      graph += path

      graph.changes must contain(exactly(
        AddPath(path).asInstanceOf[GraphChange],
        AddItem(relation).asInstanceOf[GraphChange]
      )).inOrder
    }
  }

  "Node" should {
    "emit change when setting property" in {
      val node = Node(1)
      val properties = new Properties(node)

      properties("key") = "value"
      properties += ("key" -> "value")

      properties.localChanges must contain(exactly(
        SetProperty(node, "key", "value").asInstanceOf[GraphChange],
        SetProperty(node, "key", "value").asInstanceOf[GraphChange]
      ))
    }

    "emit change when setting property" in {
      val node = Node.create
      val properties = new Properties(node)

      properties("key") = "value"
      properties += ("key" -> "value")

      properties.localChanges.size mustEqual 0
    }

    "emit change when removing property" in {
      val node = Node(1)
      val properties = new Properties(node)

      properties -= "key"

      properties.localChanges must contain(exactly(
        RemoveProperty(node, "key").asInstanceOf[GraphChange]
      ))
    }

    "not emit change when removing property on local node" in {
      val node = Node.create
      val properties = new Properties(node)

      properties -= "key"

      properties.localChanges.size mustEqual 0
    }

    "emit change when setting label" in {
      val node = Node(1)
      val labels = new NodeLabels(node)
      val label = mock[Label]

      labels += label

      labels.localChanges must contain(exactly(
        SetLabel(node, label).asInstanceOf[GraphChange]
      ))
    }

    "not emit change when setting label on local node" in {
      val node = Node.create
      val labels = new NodeLabels(node)
      val label = mock[Label]

      labels += label

      labels.localChanges.size mustEqual 0
    }

    "emit change when removing label" in {
      val node = Node(1)
      val labels = new NodeLabels(node)
      val label = mock[Label]

      labels -= label

      labels.localChanges must contain(exactly(
        RemoveLabel(node, label).asInstanceOf[GraphChange]
      ))
    }

    "not emit change when removing label on local node" in {
      val node = Node.create
      val labels = new NodeLabels(node)
      val label = mock[Label]

      labels -= label

      labels.localChanges.size mustEqual 0
    }
  }

  "Relation" should {
    "emit change when setting property" in {
      val relation = Relation(1, Node(2), Node(3), "r")
      relation.properties += ("key" -> "value")
      relation.properties -= "key"

      relation.changes must contain(
        SetProperty(relation, "key", "value")
      )
    }

    "not emit change when setting property on local relation" in {
      val relation = Relation.create(Node(2), "r", Node(3))
      relation.properties += ("key" -> "value")
      relation.properties -= "key"

      relation.changes.size mustEqual 0
    }

    "emit change when removing property" in {
      val relation = Relation(1, Node(2), Node(3), "r")
      relation.properties += ("key" -> "value")
      relation.properties -= "key"

      relation.changes must contain(
        SetProperty(relation, "key", StringPropertyValue("value")).asInstanceOf[GraphChange],
        RemoveProperty(relation, "key").asInstanceOf[GraphChange]
      ).inOrder
    }

    "not emit change when removing property on local relation" in {
      val relation = Relation.create(Node(2), "r", Node(3))
      relation.properties += ("key" -> "value")
      relation.properties -= "key"

      relation.changes.size mustEqual 0
    }
  }
}
