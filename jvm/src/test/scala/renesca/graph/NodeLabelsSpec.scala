package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.Label

@RunWith(classOf[JUnitRunner])
class NodeLabelsSpec extends Specification {

  trait MockNode extends Scope {
    val A = Node(1)

    val label = Label("some_label")
  }

  "NodeLabels" >> {
    "store label" >> new MockNode {
      A.labels += label

      A.labels must contain(exactly(label))
    }

    "remove label" >> new MockNode {
      A.labels += label

      A.labels -= label

      A.labels must beEmpty
    }

    "not contain label" >> new MockNode {
      A.labels.contains(label) must beFalse
    }

    "contain label" >> new MockNode {
      A.labels += label

      A.labels.contains(label) must beTrue
    }

    "provide iterator" >> new MockNode {
      A.labels += label

      A.labels.iterator must contain(exactly(label))
    }

    "provide empty" >> new MockNode {
      A.labels += label

      A.labels.empty must beEmpty
    }

    "clear" >> new MockNode {
      A.labels ++= Set(Label("POST"), Label("TIMESTAMP"), Label("CONNECTABLE"), Label("EXPOSEDNODE"), Label("UUIDNODE"), Label("HIDEABLE"))

      A.labels.clear()

      A.labels must beEmpty
    }
  }
}
