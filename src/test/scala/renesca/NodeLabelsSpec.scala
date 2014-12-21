package renesca

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope

import scala.collection.mutable

class NodeLabelsSpec extends Specification with Mockito {

  trait MockNode extends Scope {
    val A = Node(1)
    A.graph = mock[Graph]
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
