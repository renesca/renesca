package renesca

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope

class NodeSpec extends Specification with Mockito {

  "Node" should {
    "pass on graph reference to labels-Set and properties-Map" in {
      val node = Node(1)

      node.labels.graph mustEqual null
      node.properties.graph mustEqual null

      node.graph = mock[Graph]

      node.labels.graph mustEqual node.graph
      node.properties.graph mustEqual node.graph
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

      val graph = Graph(List(A,B,C), List(ArB, ArC, BrC))
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

    "delete itself from graph" in {
      todo
    }

    "delete incident relations from graph" in {
      todo
    }

    "be equal to other nodes with same id" in {
      todo
    }

    "not be equal to other nodes different id" in {
      todo
    }

    "have the same hashcode as nodes with the same id" in {
      todo
    }

    "not have the same hashcode as nodes with a different id" in {
      todo
    }
  }
}

