package renesca.graph

import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.specification.Scope
import renesca._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

class PropertiesSpec extends Specification with Mockito {

  implicit def toJson[T: Encoder](x: T) = x.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  trait MockNode extends Scope {
    val A = Node(1)
    val graph = mock[Graph]
  }

  "Properties" >> {
    "store property" >> new MockNode {
      A.properties("key") = "value"

      A.properties("key").asString.get mustEqual "value"
    }

    "remove property" >> new MockNode {
      A.properties("key") = "value"

      A.properties -= "key"

      A.properties.isDefinedAt("key") must beFalse
    }

    "get non-existing element" >> new MockNode {
      A.properties.get("key") mustEqual None
    }

    "get existing element" >> new MockNode {
      A.properties("key") = "value"

      A.properties.get("key").flatMap(_.asString) mustEqual Some("value")
    }

    "provide iterator" >> new MockNode {
      A.properties("key") = "value"

      A.properties.iterator must contain(exactly(PropertyKey("key") -> "value".asJson))
    }

    "provide empty" >> new MockNode {
      A.properties("key") = "value"

      A.properties.empty must beEmpty
    }
  }
}
