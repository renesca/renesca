package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.parameter.implicits._
import renesca.parameter.{PropertyValue, StringPropertyValue}

@RunWith(classOf[JUnitRunner])
class NodeSpec extends Specification with Mockito {

  "Node" should {

    "create Node with labels and properties" in {
      val label = mock[Label]
      val A = Node(1, labels = List(label), properties = Map("key" -> "value"))

      A.labels must contain(exactly(label))
      A.properties must contain(exactly("key" -> StringPropertyValue("value").asInstanceOf[PropertyValue]))
    }

    "pass on node to labels-Set and properties-Map" in {
      val node = Node(5)

      node.labels.node mustEqual node
      node.properties.item mustEqual node
    }

    "be equal to other nodes with same id" in {
      Node(1) mustEqual Node(1L)
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

    trait ForwardTest extends Scope {
      val graph = mock[Graph]
      val node = Node(1)
    }

    "ask graph for in-relations" in new ForwardTest {
      node.inRelations(graph)
      there was one(graph).inRelations(node)
    }

    "ask graph for out-relations" in new ForwardTest {
      node.outRelations(graph)
      there was one(graph).outRelations(node)
    }

    "ask graph for relations" in new ForwardTest {
      node.relations(graph)
      there was one(graph).incidentRelations(node)
    }

    "ask graph for neighbors" in new ForwardTest {
      node.neighbours(graph)
      there was one(graph).neighbours(node)
    }

    "ask graph for predecessors" in new ForwardTest {
      node.predecessors(graph)
      there was one(graph).predecessors(node)
    }

    "ask graph for successors" in new ForwardTest {
      node.successors(graph)
      there was one(graph).successors(node)
    }

    "ask graph for inDegree" in new ForwardTest {
      node.inDegree(graph)
      there was one(graph).inDegree(node)
    }

    "ask graph for outDegree" in new ForwardTest {
      node.outDegree(graph)
      there was one(graph).outDegree(node)
    }

    "ask graph for degree" in new ForwardTest {
      node.degree(graph)
      there was one(graph).degree(node)
    }

    "produce a string representation" in {
      val node = Node(15)
      node.toString mustEqual "(15)"
    }

    "produce a string representation with labels" in {
      val node = Node(15, List("A", "B"))
      node.toString mustEqual "(15:A:B)"
    }

    "produce a string representation with create" in {
      val node = Node.create
      node.toString mustEqual "(Create())"
    }

    "produce a string representation with merge" in {
      val node = Node.merge(merge = Set("a"), onMatch = Set("b"))
      node.toString mustEqual "(Merge(Set(a), Set(b)))"
    }

    "produce a string representation with matches" in {
      val node = Node.matches(matches = Set("a"))
      node.toString mustEqual "(Match(Set(a)))"
    }
  }
}

