package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope

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

    "merge graphs" in new ExampleGraph {
      val D = Node(14)
      val E = Node(15)
      val F = Node(16)
      val DrE = Relation(17, D, E)
      val DrF = Relation(18, D, F)

      val otherNodes = List(D, E, F)
      val otherRelations = List(DrE, DrF)
      val otherGraph = Graph(otherNodes, otherRelations)
      val newGraph = graph merge otherGraph

      newGraph.nodes must containAllOf(nodesList)
      newGraph.nodes must containAllOf(otherNodes)
      newGraph.relations must containAllOf(relationsList)
      newGraph.relations must containAllOf(otherRelations)
    }

    "merge graphs with shared nodes and relations" in new ExampleGraph {
      // D == A
      // E == B
      // DrE == ArB

      val D = Node(1)
      val E = Node(2)
      val F = Node(16)
      val DrE = Relation(4, D, E)
      val DrF = Relation(18, D, F)

      val otherNodes = List(D, E, F)
      val otherRelations = List(DrE, DrF)
      val otherGraph = Graph(otherNodes, otherRelations)
      val newGraph = graph merge otherGraph

      newGraph.nodes must contain(exactly(A,B,C,F))
      newGraph.relations must contain(exactly(ArB, ArC, BrC, DrF))
    }

    "merge graphs with changes" in {
      val graph1 = Graph()
      val graph2 = Graph()

      val change1 = mock[GraphChange]
      val change2 = mock[GraphChange]

      graph1.localChanges += change1
      graph2.localChanges += change2

      (graph1 merge graph2).changes must contain(exactly(change1, change2))
    }
  }
}
