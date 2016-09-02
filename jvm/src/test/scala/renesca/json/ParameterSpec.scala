package renesca.json

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ParameterProtocol._
import renesca.parameter._
import renesca.parameter.implicits._
import spray.json._

@RunWith(classOf[JUnitRunner])
class ParameterSpec extends Specification {
  "Property" can {
    def testFromAndToJson(json: String, propertyValue: PropertyValue) = {
      val jsonAst = json.parseJson
      val convertedPropertyValue = jsonAst.convertTo[PropertyValue]

      convertedPropertyValue mustEqual propertyValue
      propertyValue.toJson mustEqual jsonAst
    }

    "have a long value" in { testFromAndToJson("1744", LongPropertyValue(1744)) }
    "have a double value" in { testFromAndToJson("17.44", DoublePropertyValue(17.44)) }
    "have a string value" in { testFromAndToJson( """ "value" """, StringPropertyValue("value")) }
    "have a boolean value" in { testFromAndToJson("true", BooleanPropertyValue(true)) }

    "have an array of longs" in { testFromAndToJson("[1744, 1516]", LongArrayPropertyValue(1744, 1516)) }
    "have an array of doubles" in { testFromAndToJson("[17.44, 15.16]", DoubleArrayPropertyValue(17.44, 15.16)) }
    "have an array of strings" in { testFromAndToJson( """["17.44", "15.16"] """, StringArrayPropertyValue("17.44", "15.16")) }
    "have an array of booleans" in { testFromAndToJson( """[true, false] """, BooleanArrayPropertyValue(true, false)) }
  }

  "Parameter" can {
    def testFromAndToJson(value: String, parameterValue: ParameterValue) = {
      val jsonAst = s"""{"key":$value}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]

      parameters mustEqual Map(PropertyKey("key") -> parameterValue)
      parameters.toJson mustEqual jsonAst
    }

    "have arrays of doubles" in { testFromAndToJson("[17.44, 15.16]", List(17.44, 15.16)) }
    "have arrays of longs" in { testFromAndToJson("[1744, 1516]", List(1744, 1516)) }
    "have arrays of strings" in { testFromAndToJson( """["1744", "1516"]""", List("1744", "1516")) }
    "have arrays of booleans" in { testFromAndToJson("[true, false]", List(true, false)) }
    "have nested objects" in { testFromAndToJson( """{"foo":1,"bar":{"id":2}}""", Map("foo" -> 1, "bar" -> Map("id" -> 2))) }
    "have objects and literals in array in objects" in {
      val jsonAst = """{"a":false, "key":[{"x":9999999999999},{"y":{"frei":"bier"}},{"brei": 3.14141414}, 7]}""".parseJson
      val parameters = jsonAst.convertTo[ParameterMap]
      val parameterValue: ParameterMap = Map(
        "a" -> false,
        "key" -> ArrayParameterValue(List(
          Map("x" -> 9999999999999L),
          Map("y" -> Map("frei" -> "bier")),
          Map("brei" -> 3.14141414),
          7
        )))

      parameters mustEqual parameterValue
      parameters.toJson mustEqual jsonAst
    }
  }
}
