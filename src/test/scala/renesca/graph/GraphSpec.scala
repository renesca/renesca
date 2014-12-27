package renesca.graph

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GraphSpec extends Specification with Mockito {

  trait ExampleGraph extends Scope {
      // A-->B-->C
      //  \_____7
      val A = Node(1)
      val B = Node(2)
      val C = Node(3)
      val ArB = Relation(4, A, B)
      val ArC = Relation(5, A, C)
      val BrC = Relation(6, B, C)
      
      val nodesList = List(A,B,C)
      val relationsList = List(ArB, ArC, BrC)

      implicit val graph = Graph(nodesList, relationsList)
    }
  
  "Graph" should {
    "create graphs with nodes and relations" in new ExampleGraph {
    	import graph._
      nodes must containTheSameElementsAs(nodesList)
      relations must containTheSameElementsAs(relationsList)
    }

    "provide access to relations" in new ExampleGraph {
      import graph._

      outRelations(A) must contain(exactly(ArB, ArC))
      outRelations(B) must contain(exactly(BrC))
      outRelations(C) must beEmpty

      inRelations(A) must beEmpty
      inRelations(B) must contain(ArB)
      inRelations(C) must contain(exactly(BrC, ArC))

      incidentRelations(A) must contain(exactly(ArB, ArC))
      incidentRelations(B) must contain(exactly(ArB, BrC))
      incidentRelations(C) must contain(exactly(ArC, BrC))
    }

    "provide access to neighbours" in new ExampleGraph {
      import graph._
      
      predecessors(A) must beEmpty
      predecessors(B) must contain(exactly(A))
      predecessors(C) must contain(exactly(A,B))

      successors(A) must contain(exactly(B,C))
      successors(B) must contain(exactly(C))
      successors(C) must beEmpty

      neighbours(A) must contain(exactly(B,C))
      neighbours(B) must contain(exactly(A,C))
      neighbours(C) must contain(exactly(A,B))
    }

    "provide access to degrees" in new ExampleGraph {
      import graph._
      
      inDegree(A) mustEqual 0
      inDegree(B) mustEqual 1
      inDegree(C) mustEqual 2

      outDegree(A) mustEqual 2
      outDegree(B) mustEqual 1
      outDegree(C) mustEqual 0

      degree(A) mustEqual 2
      degree(B) mustEqual 2
      degree(C) mustEqual 2
    }

    "delete itself from graph" in new ExampleGraph {
      import graph._
      
      delete(B)

      nodes must contain(exactly(A,C))
      relations must contain(exactly(ArC))
    }

  }
}
