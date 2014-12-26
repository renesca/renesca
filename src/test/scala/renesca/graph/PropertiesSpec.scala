package renesca.graph

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import renesca.graph.helpers.Properties

import scala.collection.mutable

class PropertiesSpec extends Specification with Mockito {

  trait MockNode extends Scope {
    val A = Node(1)
    val graph = mock[Graph]
    A._graph = graph
    A._properties = new Properties(A.id, mock[(Long,String,PropertyValue) => GraphChange], mock[(Long,String) => GraphChange])
    A._properties._graph = graph
    A.graph.changes returns mock[mutable.ArrayBuffer[GraphChange]]

    val label = mock[Label]
  }

  "Properties" should {
    "store property" in new MockNode {
      A.properties("key") = "value"

      A.properties("key") mustEqual StringPropertyValue("value")
    }

    "remove property" in new MockNode {
      A.properties("key") = "value"

      A.properties -= "key"

      A.properties.isDefinedAt("key") must beFalse
    }

    "get non-existing element" in new MockNode {
      A.properties.get("key") mustEqual None
    }

    "get existing element" in new MockNode {
      A.properties("key") = "value"

      A.properties.get("key") mustEqual Some(StringPropertyValue("value"))
    }

    "provide iterator" in new MockNode {
      A.properties("key") = "value"

      A.properties.iterator must contain(exactly("key" -> StringPropertyValue("value").asInstanceOf[PropertyValue]))
    }

    "provide empty"in new MockNode  {
      A.properties("key") = "value"

      A.properties.empty must beEmpty
    }

  }

}
