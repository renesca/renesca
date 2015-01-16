package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.json.StringPropertyValue

@RunWith(classOf[JUnitRunner])
class GraphManagerDbSpec extends IntegrationSpecification {

  "GraphManager.persist" should {

    trait ExampleNode extends Scope {
      val graph = db.queryGraph("create n return n")
      val node = graph.nodes.head

      def resultNode = {
        val resultGraph = db.queryGraph("match n return n")
        graph.nodes.head
      }
    }

    "set string property on node" in new ExampleNode {
      node.properties("schnipp") = "schnapp"
      graphManager.persistChanges(graph)

      resultNode.properties("schnipp") mustEqual StringPropertyValue("schnapp")
    }
  }

}

