package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

@RunWith(classOf[JUnitRunner])
class NodeSpec extends Specification with Mockito {
  implicit def toJson[T: Encoder](x: T) = x.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  "Node" >> {
    "be empty" >> {
      val json = """
       {
         "id" : "1",
         "labels" : [],
         "properties" : {}
       } """
      val node = decode[Node](json)
      node mustEqual Right(Node("1"))
    }
  }
  "have properties" >> {
    val json = """
       {
         "id" : "1",
         "labels" : [],
         "properties" : {
          "key" : "value",
          "key2" : "value2"
         }
       } """
    val node = decode[Node](json)
    node mustEqual Right(Node("1", Nil, Map(
      "key" -> "value",
      "key2" -> "value2"
    )))
  }

  "have properties of different types" >> {
    val json = """
       {
         "id" : "1",
         "labels" : [],
         "properties" : {
          "key" : "value",
          "key2" : 1
         }
       } """
    val node = decode[Node](json)
    node mustEqual Right(Node("1", Nil, Map(
      "key" -> "value",
      "key2" -> 1
    )))
  }

  "have labels" >> {
    val json = """
       {
         "id" : "1",
         "labels" : ["bier", "1516"],
         "properties" : {}
       } """
    val node = decode[Node](json)
    node mustEqual Right(Node("1", List("bier", "1516"), Map.empty))
  }
}
