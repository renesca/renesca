package renesca.json

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ValueProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class ParametersSpec extends Specification {
  "Paramaters" can {
    "have nested objects" in {
      val jsonAst = """{"key":{"foo":1,"bar":{"id":2}}}""".parseJson
      val properties = jsonAst.convertTo[Map[String, ParameterValue]]

      properties mustEqual Map("key" -> MapParameterValue(Map("foo" -> LongPropertyValue(1), "bar" -> MapParameterValue(Map("id" -> LongPropertyValue(2))))))
      properties.toJson mustEqual jsonAst
    }
  }
}
