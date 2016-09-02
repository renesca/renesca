package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope

@RunWith(classOf[JUnitRunner])
class NodeLabelsSpec extends Specification with Mockito {

  trait MockNode extends Scope {
    val A = Node(1)

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

    "clear" in new MockNode {
      A.labels ++= Set(Label("POST"),Label("TIMESTAMP"),Label("CONNECTABLE"),Label("EXPOSEDNODE"),Label("UUIDNODE"),Label("HIDEABLE"))

      A.labels.clear()

      A.labels must beEmpty
    }
  }
}
