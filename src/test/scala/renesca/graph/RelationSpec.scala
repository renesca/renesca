package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph.Id._
import renesca.parameter.implicits._

@RunWith(classOf[JUnitRunner])
class RelationSpec extends Specification with Mockito {

  "Relation" should {
    "pass on relation id to properties-Map" in {
      val relation = Relation(5, mock[Node], mock[Node], "r")

      relation.properties.item mustEqual relation
    }

    trait ExampleGraph extends Scope {
      // A-->B-->C
      //  \_____7
      val A = Node(1)
      val B = Node(2)
      val C = Node(3)
      val ArB = Relation(4, A, B, "r")
      val ArC = Relation(5, A, C, "r")
      val BrC = Relation(6, B, C, "r")

      implicit val graph = Graph(List(A, B, C), List(ArB, ArC, BrC))
    }

    "delete itself from graph" in new ExampleGraph {
      graph.relations -= ArB

      graph.nodes must contain(exactly(A, B, C))
      graph.relations must contain(exactly(ArC, BrC))
    }

    trait MockNodes extends Scope {
      val A = mock[Node]
      val B = mock[Node]
    }

    "provide access to other Node" in new MockNodes {
      val ArB = Relation(1, A, B, "r")

      ArB.other(A) mustEqual B
      ArB.other(B) mustEqual A
    }

    "be equal to other relations with same id" in new MockNodes {
      Relation(1, A, B, "r") mustEqual Relation(1, A, B, RelationType("r"))
      Relation(1, A, B, "r") mustEqual Relation(1, B, A, RelationType("r"))
    }

    "not be equal to other relations different id" in new MockNodes {
      Relation(1, A, B, "r") mustNotEqual Relation(2, B, A, RelationType("r"))
      Relation(1, A, B, "r") mustNotEqual Relation(2, A, B, RelationType("r"))
    }

    "have the same hashcode as relations with the same id" in new MockNodes {
      Relation(1, A, B, "r").hashCode mustEqual Relation(1, A, B, RelationType("r")).hashCode
      Relation(1, A, B, "r").hashCode mustEqual Relation(1, B, A, RelationType("r")).hashCode
    }

    "not have the same hashcode as relations with a different id" in new MockNodes {
      Relation(1, A, B, "r").hashCode mustNotEqual Relation(2, B, A, RelationType("r")).hashCode
      Relation(1, A, B, "r").hashCode mustNotEqual Relation(2, A, B, RelationType("r")).hashCode
    }

    "produce string representation" in {
      val relation = Relation(10, Node(5), Node(7), "R")
      relation.toString mustEqual "(5)-[10:R]->(7)"
    }

    "produce string representation with node labels" in {
      val relation = Relation(10, Node(5, List("A", "B")), Node(7, List("C")), "R")
      relation.toString mustEqual "(5:A:B)-[10:R]->(7:C)"
    }

    "produce string representation with create" in {
      val relation = Relation.create(Node(5), "R", Node(7))
      relation.toString mustEqual "(5)-[Create():R]->(7)"
    }

    "produce string representation with merge" in {
      val relation = Relation.merge(Node(5), "R", Node(7), merge = Set("a"), onMatch = Set("b"))
      relation.toString mustEqual "(5)-[Merge(Set(a), Set(b)):R]->(7)"
    }

    "produce string representation with matches" in {
      val relation = Relation.matches(Node(5), "R", Node(7), matches = Set("a"))
      relation.toString mustEqual "(5)-[Match(Set(a)):R]->(7)"
    }
  }
}

