package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

@RunWith(classOf[JUnitRunner])
class NodeSpec extends Specification with Mockito {

  implicit def intToJson(x: Int) = x.asJson
  implicit def stringToJson(x: String) = x.asJson
  implicit def listToJson[T: Encoder](xs: List[T]) = xs.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  "Node" >> {

    "create Node with labels and properties" >> {
      val label = Label("dietrich")
      val A = Node(1, labels = List(label), properties = Map("key" -> "value"))

      A.labels must contain(exactly(label))
      A.properties must contain(exactly(NonBacktickName("key") -> "value".asJson))
    }

    "pass on node to labels-Set and properties-Map" >> {
      val node = Node(5)

      node.labels.node mustEqual node
      node.properties.item mustEqual node
    }

    "be equal to other nodes with same id" >> {
      Node(1) mustEqual Node(1L)
    }

    "not be equal to other nodes different id" >> {
      Node(1) mustNotEqual Node(2)
    }

    "have the same hashcode as nodes with the same id" >> {
      Node(1).hashCode mustEqual Node(1).hashCode
    }

    "not have the same hashcode as nodes with a different id" >> {
      Node(1).hashCode mustNotEqual Node(2).hashCode
    }

    trait ForwardTest extends Scope {
      val graph = mock[Graph]
      val node = Node(1)
    }

    "ask graph for in-relations" >> new ForwardTest {
      node.inRelations(graph)
      there was one(graph).inRelations(node)
    }

    "ask graph for out-relations" >> new ForwardTest {
      node.outRelations(graph)
      there was one(graph).outRelations(node)
    }

    "ask graph for relations" >> new ForwardTest {
      node.relations(graph)
      there was one(graph).incidentRelations(node)
    }

    "ask graph for neighbors" >> new ForwardTest {
      node.neighbours(graph)
      there was one(graph).neighbours(node)
    }

    "ask graph for predecessors" >> new ForwardTest {
      node.predecessors(graph)
      there was one(graph).predecessors(node)
    }

    "ask graph for successors" >> new ForwardTest {
      node.successors(graph)
      there was one(graph).successors(node)
    }

    "ask graph for inDegree" >> new ForwardTest {
      node.inDegree(graph)
      there was one(graph).inDegree(node)
    }

    "ask graph for outDegree" >> new ForwardTest {
      node.outDegree(graph)
      there was one(graph).outDegree(node)
    }

    "ask graph for degree" >> new ForwardTest {
      node.degree(graph)
      there was one(graph).degree(node)
    }

    "produce a string representation" >> {
      val node = Node(15)
      node.toString mustEqual "(15)"
    }

    "produce a string representation with labels" >> {
      val node = Node(15, List("A", "B"))
      node.toString mustEqual "(15:A:B)"
    }

    "produce a string representation with create" >> {
      val node = Node.create
      node.toString mustEqual "(Create())"
    }

    "produce a string representation with merge" >> {
      val node = Node.merge(merge = Set("a"), onMatch = Set("b"))
      node.toString mustEqual "(Merge(Set(a), Set(b)))"
    }

    "produce a string representation with matches" >> {
      val node = Node.matches(matches = Set("a"))
      node.toString mustEqual "(Match(Set(a)))"
    }
  }
}
