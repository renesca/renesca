package renesca.json

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ValueProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class PropertiesSpec extends Specification {
  "PropertyValue" can {
    def testFromAndToJson(json:String, propertyValue:PropertyValue) = {
      val jsonAst = json.parseJson
      val convertedPropertyValue = jsonAst.convertTo[PropertyValue]

      convertedPropertyValue mustEqual propertyValue
      propertyValue.toJson mustEqual jsonAst
    }

    "have a string value" in { testFromAndToJson(""" "value" """, StringPropertyValue("value")) }
    "have a boolean value" in { testFromAndToJson("true", BooleanPropertyValue(true)) }
    "have a double value" in { testFromAndToJson("17.44", DoublePropertyValue(17.44)) }
    "have a long value" in { testFromAndToJson("1744", LongPropertyValue(1744)) }
    "have a null value" in { testFromAndToJson("null", NullPropertyValue) }
    "be null" in { testFromAndToJson("null", null) }

    "have an array of longs" in { testFromAndToJson("[1744, 1516]", ArrayPropertyValue(List(LongPropertyValue(1744), LongPropertyValue(1516)))) }
    "have an array of doubles" in { testFromAndToJson("[17.44, 15.16]", ArrayPropertyValue(List(DoublePropertyValue(17.44), DoublePropertyValue(15.16)))) }
    "have an array of strings" in { testFromAndToJson("""["17.44", "15.16"] """, ArrayPropertyValue(List(StringPropertyValue("17.44"), StringPropertyValue("15.16")))) }
    "have an array of booleans" in { testFromAndToJson("""[true, false] """, ArrayPropertyValue(List(BooleanPropertyValue(true), BooleanPropertyValue(false)))) }
    "have an array of nulls" in { testFromAndToJson("[null, null]", ArrayPropertyValue(List(NullPropertyValue, NullPropertyValue))) }
  }
}
