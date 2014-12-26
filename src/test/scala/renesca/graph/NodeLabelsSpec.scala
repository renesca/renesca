package renesca.graph

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import renesca.graph.helpers.NodeLabels

import scala.collection.mutable

class NodeLabelsSpec extends Specification with Mockito {

  trait MockNode extends Scope {
    val A = Node(1)
    val graph = mock[Graph]
    A._graph = graph
    A._labels = new NodeLabels(A.id)
    A._labels._graph = graph
    A.graph.changes returns mock[mutable.ArrayBuffer[GraphChange]]

    val label = mock[Label]
  }

  "NodeLabels" should {
    "store label" in new MockNode {
      A.labels += label

      A.labels must contain(exactly(label))
    }

    "remove label" in new MockNode {
      A.labels += label

      A.labels -= label

      A.labels must beEmpty
    }

    "not contain label" in new MockNode {
      A.labels.contains(label) must beFalse
    }

    "contain label" in new MockNode {
      A.labels += label

      A.labels.contains(label) must beTrue
    }

    "provide iterator" in new MockNode {
      A.labels += label

      A.labels.iterator must contain(exactly(label))
    }

    "provide empty" in new MockNode {
      A.labels += label

      A.labels.empty must beEmpty
    }

  }

}
