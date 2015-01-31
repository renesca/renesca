package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ResponseJsonProtocol._
import renesca.parameter.{LongPropertyValue, PropertyKey, StringPropertyValue}
import spray.json._

@RunWith(classOf[JUnitRunner])
class NodeSpec extends Specification with Mockito {
  "Node" can {
    "be empty" in {
      val json = """
       {
         "id" : "1",
         "labels" : [],
         "properties" : {}
       } """
      val node = json.parseJson.convertTo[Node]
      node mustEqual Node("1")
    }
  }
  "have properties" in {
    val json = """
       {
         "id" : "1",
         "labels" : [],
         "properties" : {
          "key" : "value",
          "key2" : "value2"
         }
       } """
    val node = json.parseJson.convertTo[Node]
    node mustEqual Node("1", Nil, Map(
      PropertyKey("key") -> StringPropertyValue("value"),
      PropertyKey("key2") -> StringPropertyValue("value2"))
    )
  }

  "have properties of different types" in {
    val json = """
       {
         "id" : "1",
         "labels" : [],
         "properties" : {
          "key" : "value",
          "key2" : 1
         }
       } """
    val node = json.parseJson.convertTo[Node]
    node mustEqual Node("1", Nil, Map(
      PropertyKey("key") -> StringPropertyValue("value"),
      PropertyKey("key2") -> LongPropertyValue(1))
    )
  }

  "have labels" in {
    val json = """
       {
         "id" : "1",
         "labels" : ["bier", "1516"],
         "properties" : {}
       } """
    val node = json.parseJson.convertTo[Node]
    node mustEqual Node("1", List("bier", "1516"), Map.empty)
  }
}
