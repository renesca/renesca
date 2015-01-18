package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.json._

@RunWith(classOf[JUnitRunner])
class GraphManagerDbSpec extends IntegrationSpecification {

  def testNodeSetProperty(data:PropertyValue) = {
    val graph = db.queryGraph("create n return n")
    val node = graph.nodes.head

    node.properties("key") = data
    graphManager.persistChanges(graph)

    val resultGraph = db.queryGraph("match n return n")
    val resultNode = resultGraph.nodes.head
    resultNode.properties("key") mustEqual data
  }

  "GraphManager.persist" should {

    "set long property on node" in { testNodeSetProperty(123) }
    "set double property on node" in { testNodeSetProperty(1.337) }
    "set string property on node" in { testNodeSetProperty("schnipp") }
    "set boolean property on node" in { testNodeSetProperty(true) }

    "set long array property on node" in { testNodeSetProperty(List(1, 3)) }
    "set double array property on node" in { testNodeSetProperty(List(1.7, 2.555555)) }
    "set string array property on node" in { testNodeSetProperty(List("schnipp","schnapp")) }
    "set boolean array property on node" in { testNodeSetProperty(List(true, false)) }
  }

}

