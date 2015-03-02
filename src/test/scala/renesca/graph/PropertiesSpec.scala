package renesca.graph

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.parameter.implicits._
import renesca.parameter.{PropertyKey, PropertyValue, StringPropertyValue}

@RunWith(classOf[JUnitRunner])
class PropertiesSpec extends Specification with Mockito {

  trait MockNode extends Scope {
    val A = Node(1)
    val graph = mock[Graph]
    val label = mock[Label]
  }

  "Properties" should {
    "store property" in new MockNode {
      A.properties("key") = "value"

      A.properties("key") mustEqual "value"
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

      A.properties.get("key") mustEqual Some("value")
    }

    "provide iterator" in new MockNode {
      A.properties("key") = "value"

      A.properties.iterator must contain(exactly(PropertyKey("key") -> StringPropertyValue("value").asInstanceOf[PropertyValue]))
    }

    "provide empty" in new MockNode {
      A.properties("key") = "value"

      A.properties.empty must beEmpty
    }
  }
}
