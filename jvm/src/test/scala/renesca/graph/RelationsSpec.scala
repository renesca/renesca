package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class RelationsSpec extends Specification with Mockito {
  val graph = mock[Graph].smart
  graph.nodes returns mock[Nodes]
  graph.nodes.graph returns graph

  "Relations" >> {
    "keep graph" >> {
      "Relations +=" >> {
        val relations = Relations.empty
        relations.graph = graph

        val op = relations += Relation(3, Node(1), Node(2), "r")

        op.graph mustEqual graph
        relations.graph mustEqual graph
      }
      "Relations.remove" >> {
        val relations = Relations(Relation(3, Node(1), Node(2), "r"))
        relations.graph = graph

        relations.remove(0)
        relations.graph mustEqual graph
      }
      "Relations -=" >> {
        val relations = Relations(Relation(3, Node(1), Node(2), "r"))
        relations.graph = graph

        val op = relations -= Relation(3, Node(1), Node(2), "r")

        op.graph mustEqual graph
        relations.graph mustEqual graph
      }
      "Relations ++= List" >> {
        val relations = Relations.empty
        relations.graph = graph

        val op = relations ++= List(Relation(3, Node(1), Node(2), "r"))

        op.graph mustEqual graph
        relations.graph mustEqual graph
      }

      "Relations ++= Relations" >> {
        val relations = Relations.empty
        relations.graph = graph

        val op = relations ++= Relations(Relation(3, Node(1), Node(2), "r"))

        op.graph mustEqual graph
        relations.graph mustEqual graph
      }

      "Relations --= List" >> {
        val relations = Relations(Relation(3, Node(1), Node(2), "r"), Relation(3, Node(1), Node(2), "r"))
        relations.graph = graph

        val op = relations --= List(Relation(3, Node(1), Node(2), "r"), Relation(3, Node(1), Node(2), "r"))

        op.graph mustEqual graph
        relations.graph mustEqual graph
      }

      "Relations --= Relations" >> {
        val relations = Relations(Relation(3, Node(1), Node(2), "r"), Relation(3, Node(1), Node(2), "r"))
        relations.graph = graph

        val op = relations --= Relations(Relation(3, Node(1), Node(2), "r"), Relation(3, Node(1), Node(2), "r"))

        op.graph mustEqual graph
        relations.graph mustEqual graph
      }

      "Relations.clone" >> {
        val relations = Relations(Relation(3, Node(1), Node(2), "r"), Relation(3, Node(1), Node(2), "r"))
        relations.graph = graph
        val clone = relations.clone()
        clone.graph mustEqual graph
      }
    }
  }
}


