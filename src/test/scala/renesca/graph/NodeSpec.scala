package renesca.graph

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner


@RunWith(classOf[JUnitRunner])
class NodeSpec extends Specification with Mockito {

  "Node" should {

    "create Node with labels and properties" in {
      val label = mock[Label]
      val A = Node(1, labels = List(label), properties = Map("key" -> "value"))

      A.labels must contain(exactly(label))
      A.properties must contain(exactly("key" -> StringPropertyValue("value").asInstanceOf[PropertyValue]))
    }

    "pass on node id to labels-Set and properties-Map" in {
      val nodeId = 5
      val node = Node(nodeId)

      node.labels.id mustEqual nodeId
      node.properties.id mustEqual nodeId
    }

    trait ExampleGraph extends Scope {
      // A-->B-->C
      //  \_____7
      val A = Node(1)
      val B = Node(2)
      val C = Node(3)
      val ArB = Relation(4, A, B)
      val ArC = Relation(5, A, C)
      val BrC = Relation(6, B, C)

      implicit val graph = Graph(List(A,B,C), List(ArB, ArC, BrC))
    }

    "provide access to relations" in new ExampleGraph {
      A.outRelations must contain(exactly(ArB, ArC))
      B.outRelations must contain(exactly(BrC))
      C.outRelations must beEmpty

      A.inRelations must beEmpty
      B.inRelations must contain(ArB)
      C.inRelations must contain(exactly(BrC, ArC))

      A.relations must contain(exactly(ArB, ArC))
      B.relations must contain(exactly(ArB, BrC))
      C.relations must contain(exactly(ArC, BrC))
    }

    "provide access to neighbours" in new ExampleGraph {
      A.predecessors must beEmpty
      B.predecessors must contain(exactly(A))
      C.predecessors must contain(exactly(A,B))

      A.successors must contain(exactly(B,C))
      B.successors must contain(exactly(C))
      C.successors must beEmpty

      A.neighbours must contain(exactly(B,C))
      B.neighbours must contain(exactly(A,C))
      C.neighbours must contain(exactly(A,B))
    }

    "provide access to degrees" in new ExampleGraph {
      A.inDegree mustEqual 0
      B.inDegree mustEqual 1
      C.inDegree mustEqual 2

      A.outDegree mustEqual 2
      B.outDegree mustEqual 1
      C.outDegree mustEqual 0

      A.degree mustEqual 2
      B.degree mustEqual 2
      C.degree mustEqual 2
    }

    "delete itself from graph" in new ExampleGraph {
      B.delete

      graph.nodes must contain(exactly(A,C))
    }

    "delete incident relations from graph" in new ExampleGraph {
      B.delete

      graph.relations must contain(exactly(ArC))
    }

    "be equal to other nodes with same id" in {
      Node(1) mustEqual Node(1)
    }

    "not be equal to other nodes different id" in {
      Node(1) mustNotEqual Node(2)
    }

    "have the same hashcode as nodes with the same id" in {
      Node(1).hashCode mustEqual Node(1).hashCode
    }

    "not have the same hashcode as nodes with a different id" in {
      Node(1).hashCode mustNotEqual Node(2).hashCode
    }
  }
}

