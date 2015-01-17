package renesca.json

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ValueProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class PropertiesSpec extends Specification {
  "Properties" can {


    def testFromJson(json:String, result:PropertyValue) = {
      val jsonAst = s"""{"key":$json}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> result)
      properties.toJson mustEqual jsonAst
    }

    "have a string value" in { testFromJson(""" "value" """, StringPropertyValue("value")) }
    "have a boolean value" in { testFromJson("true", BooleanPropertyValue(true)) }
    "have a double value" in { testFromJson("17.44", DoublePropertyValue(17.44)) }
    "have a long value" in { testFromJson("1744", LongPropertyValue(1744)) }
    "have a null value" in { testFromJson("null", NullPropertyValue) }

    "have different value types" in {
      val jsonAst = """{"key":1744, "key2" : "bier"}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> LongPropertyValue(1744), "key2" -> StringPropertyValue("bier"))
      properties.toJson mustEqual jsonAst
    }

    "have arrays of longs" in { testFromJson("[1744, 1516]", ArrayPropertyValue(List(LongPropertyValue(1744), LongPropertyValue(1516)))) }
    "have arrays of doubles" in { testFromJson("[17.44, 15.16]", ArrayPropertyValue(List(DoublePropertyValue(17.44), DoublePropertyValue(15.16)))) }
    "have arrays of strings" in { testFromJson("""["17.44", "15.16"] """, ArrayPropertyValue(List(StringPropertyValue("17.44"), StringPropertyValue("15.16")))) }
    "have arrays of booleans" in { testFromJson("""[true, false] """, ArrayPropertyValue(List(BooleanPropertyValue(true), BooleanPropertyValue(false)))) }
    "have arrays of null" in { testFromJson("[null, null]", ArrayPropertyValue(List(NullPropertyValue, NullPropertyValue))) }

    "have different arrays" in {
      val jsonAst = """{"key":[null, null], "key2":[1,-2]}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map(
        "key" -> ArrayPropertyValue(List(NullPropertyValue, NullPropertyValue)),
        "key2" -> ArrayPropertyValue(List(LongPropertyValue(1), LongPropertyValue(-2))))
      properties.toJson mustEqual jsonAst
    }
  }
}
