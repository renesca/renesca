package renesca.json

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.ParameterValue.ParameterMap
import renesca.json.protocols.ValueProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class ParameterValueSpec extends Specification {
  "Parameters" can {
    "have arrays of doubles" in {
      val jsonAst = """{"key":[17.44, 15.16]}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("key") -> ArrayParameterValue(List(DoublePropertyValue(17.44), DoublePropertyValue(15.16))))
      parameters.toJson mustEqual jsonAst
    }

    "have arrays of longs" in {
      val jsonAst = """{"key":[1744, 1516]}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("key") -> ArrayParameterValue(List(LongPropertyValue(1744), LongPropertyValue(1516))))
      parameters.toJson mustEqual jsonAst
    }

    "have arrays of strings" in {
      val jsonAst = """{"key":["1744", "1516"]}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("key") -> ArrayParameterValue(List(StringPropertyValue("1744"), StringPropertyValue("1516"))))
      parameters.toJson mustEqual jsonAst
    }

    "have arrays of booleans" in {
      val jsonAst = """{"key":[true, false]}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("key") -> ArrayParameterValue(List(BooleanPropertyValue(true), BooleanPropertyValue(false))))
      parameters.toJson mustEqual jsonAst
    }

    "have nested objects" in {
      val jsonAst = """{"key":{"foo":1,"bar":{"id":2}}}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("key") -> MapParameterValue(
        Map(PropertyKey("foo") -> LongPropertyValue(1), PropertyKey("bar") -> MapParameterValue(
          Map(PropertyKey("id") -> LongPropertyValue(2))
        ))
      ))
      parameters.toJson mustEqual jsonAst
    }

    "have objects and literals in array in objects" in {
      val jsonAst = """{"a":false, "key":[{"x":9999999999999},{"y":{"frei":"bier"}},{"brei": 3.14141414}, 7]}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("a") -> BooleanPropertyValue(false), PropertyKey("key") -> ArrayParameterValue(List(
        MapParameterValue(Map(PropertyKey("x") -> LongPropertyValue(9999999999999L))),
        MapParameterValue(Map(PropertyKey("y") -> MapParameterValue(Map(PropertyKey("frei") -> StringPropertyValue("bier"))))),
        MapParameterValue(Map(PropertyKey("brei") -> DoublePropertyValue(3.14141414))),
        LongPropertyValue(7)
      )))
      parameters.toJson mustEqual jsonAst
    }
  }
}
