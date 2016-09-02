package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.DistinctBuffer
import renesca.parameter.implicits._
import renesca.parameter.{PropertyValue, StringPropertyValue}

@RunWith(classOf[JUnitRunner])
class NodesSpec extends Specification with Mockito {
  val graph = mock[Graph].smart
  graph.relations returns mock[Relations]
  graph.relations.graph returns graph

  "Nodes" >> {
    "keep graph" >> {
      "Nodes +=" >> {
        val nodes = Nodes.empty
        nodes.graph = graph

        val op = nodes += Node(1)

        op.graph mustEqual graph
        nodes.graph mustEqual graph
      }
      "Nodes.remove" >> {
        val nodes = Nodes(Node(1))
        nodes.graph = graph

        nodes.remove(0)
        nodes.graph mustEqual graph
      }
      "Nodes -=" >> {
        val nodes = Nodes(Node(1))
        nodes.graph = graph

        val op = nodes -= Node(1)

        op.graph mustEqual graph
        nodes.graph mustEqual graph
      }
      "Nodes ++= List" >> {
        val nodes = Nodes.empty
        nodes.graph = graph

        val op = nodes ++= List(Node(1), Node(2))

        op.graph mustEqual graph
        nodes.graph mustEqual graph
      }

      "Nodes ++= Nodes" >> {
        val nodes = Nodes.empty
        nodes.graph = graph

        val op = nodes ++= Nodes(Node(1), Node(2))

        op.graph mustEqual graph
        nodes.graph mustEqual graph
      }

      "Nodes --= List" >> {
        val nodes = Nodes(Node(1), Node(2))

        nodes.graph = graph

        val op = nodes --= List(Node(1), Node(2))

        op.graph mustEqual graph
        nodes.graph mustEqual graph
      }

      "Nodes --= Nodes" >> {
        val nodes = Nodes(Node(1), Node(2))
        nodes.graph = graph

        val op = nodes --= List(Node(1), Node(2))

        op.graph mustEqual graph
        nodes.graph mustEqual graph
      }

      "Nodes.clone" >> {
        val nodes = Nodes(Node(1), Node(2))
        nodes.graph = graph
        val clone = nodes.clone()
        clone.graph mustEqual graph
        clone must beAnInstanceOf[Nodes]
      }

      "Nodes.take" >> {
        val nodes = Nodes(Node(1), Node(2))
        nodes.graph = graph
        val take = nodes.take(1)
        take must contain(exactly(Node(1)))
        take must beAnInstanceOf[Nodes]
        take.asInstanceOf[Nodes].graph mustEqual graph
      }

      "Nodes.map" >> {
        val nodes = Nodes(Node(1, labels = List("A")), Node(2, labels = List("B")))
        nodes.graph = graph
        val map = nodes.map(_.labels.head)
        map must beAnInstanceOf[DistinctBuffer[String]]
      }.pendingUntilFixed

      "graph creation" >> {
        val graph = Graph(List(Node(1), Node(2)), List(Relation(3, Node(1), Node(2), "r")))
        graph.nodes.graph mustEqual graph
        graph.relations.graph mustEqual graph

      }

      "graph.merge" >> {
        val graph1 = Graph(List(Node(1), Node(2)), List(Relation(3, Node(1), Node(2), "r")))
        val graph2 = Graph(List(Node(4), Node(5)), List(Relation(6, Node(4), Node(5), "r")))
        val merged = graph1 merge graph2
        merged.nodes.graph mustEqual merged
        merged.relations.graph mustEqual merged
      }
    }
  }
}


