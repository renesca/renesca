package renesca

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope

class RelationSpec extends Specification with Mockito {

  "Relation" should {
    "pass on graph reference to properties-Map" in {
      val relation = Relation(1, mock[Node], mock[Node])

      relation.properties.graph mustEqual null

      relation.graph = mock[Graph]

      relation.properties.graph mustEqual relation.graph
    }

    "pass on relation id to properties-Map" in {
      val relationId = 5
      val relation = Relation(relationId, mock[Node], mock[Node])

      relation.properties.id mustEqual relationId
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

    "store label" in new ExampleGraph {
      val label = mock[Label]

      A.labels += label

      A.labels must contain(exactly(label))
    }

    "remove label" in new ExampleGraph {
      val label = mock[Label]
      A.labels += label

      A.labels -= label

      A.labels must beEmpty
    }

    "delete itself from graph" in new ExampleGraph {
      ArB.delete()

      graph.nodes must contain(exactly(A,B,C))
      graph.relations must contain(exactly(ArC, BrC))
    }

    trait MockNodes extends Scope {
      val A = mock[Node]
      val B = mock[Node]
    }

    "provide access to other Node" in new MockNodes {
      val ArB = Relation(1, A, B)

      ArB.other(A) mustEqual B
      ArB.other(B) mustEqual A
    }


    "be equal to other relations with same id" in new MockNodes {
      Relation(1, A, B) mustEqual Relation(1, A, B)
      Relation(1, A, B) mustEqual Relation(1, B, A)
    }

    "not be equal to other relations different id" in new MockNodes {
      Relation(1, A, B) mustNotEqual Relation(2, B, A)
      Relation(1, A, B) mustNotEqual Relation(2, A, B)
    }

    "have the same hashcode as relations with the same id" in new MockNodes {
      Relation(1, A, B).hashCode mustEqual Relation(1, A, B).hashCode
      Relation(1, A, B).hashCode mustEqual Relation(1, B, A).hashCode
    }

    "not have the same hashcode as relations with a different id" in new MockNodes {
      Relation(1, A, B).hashCode mustNotEqual Relation(2, B, A).hashCode
      Relation(1, A, B).hashCode mustNotEqual Relation(2, A, B).hashCode
    }
  }
}

