package renesca.json

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ParameterProtocol._
import renesca.parameter._
import renesca.parameter.implicits._
import spray.json._

@RunWith(classOf[JUnitRunner])
class PropertyValueSpec extends Specification {
  "PropertyValue" can {
    def testFromAndToJson(json:String, propertyValue:PropertyValue) = {
      val jsonAst = json.parseJson
      val convertedPropertyValue = jsonAst.convertTo[PropertyValue]

      convertedPropertyValue mustEqual propertyValue
      propertyValue.toJson mustEqual jsonAst
    }

    "have a long value" in { testFromAndToJson("1744", LongPropertyValue(1744)) }
    "have a double value" in { testFromAndToJson("17.44", DoublePropertyValue(17.44)) }
    "have a string value" in { testFromAndToJson(""" "value" """, StringPropertyValue("value")) }
    "have a boolean value" in { testFromAndToJson("true", BooleanPropertyValue(true)) }

    "have an array of longs" in { testFromAndToJson("[1744, 1516]", ArrayPropertyValue(List(LongPropertyValue(1744), LongPropertyValue(1516)))) }
    "have an array of doubles" in { testFromAndToJson("[17.44, 15.16]", ArrayPropertyValue(List(DoublePropertyValue(17.44), DoublePropertyValue(15.16)))) }
    "have an array of strings" in { testFromAndToJson("""["17.44", "15.16"] """, ArrayPropertyValue(List(StringPropertyValue("17.44"), StringPropertyValue("15.16")))) }
    "have an array of booleans" in { testFromAndToJson("""[true, false] """, ArrayPropertyValue(List(BooleanPropertyValue(true), BooleanPropertyValue(false)))) }

    "not equal objects of different type" in {
      LongPropertyValue(1) mustNotEqual DoublePropertyValue(1)
      LongPropertyValue(1) mustNotEqual StringPropertyValue("1")
      LongPropertyValue(1) mustNotEqual BooleanPropertyValue(true)
      LongPropertyValue(1) mustNotEqual ArrayPropertyValue(List(1))

      DoublePropertyValue(1) mustNotEqual StringPropertyValue("1")
      DoublePropertyValue(1) mustNotEqual BooleanPropertyValue(true)
      DoublePropertyValue(1) mustNotEqual ArrayPropertyValue(List(1))

      StringPropertyValue("1") mustNotEqual BooleanPropertyValue(true)
      StringPropertyValue("1") mustNotEqual ArrayPropertyValue(List(1))

      BooleanPropertyValue(true) mustNotEqual ArrayPropertyValue(List(1))
    }

    "equal objects with same contents and its contents: Long" in {
      LongPropertyValue(13) mustEqual LongPropertyValue(13)
      LongPropertyValue(13) mustNotEqual LongPropertyValue(14)
      LongPropertyValue(13) mustEqual 13L
      LongPropertyValue(13) mustNotEqual 14L
      LongPropertyValue(13) mustEqual 13 // Int
      LongPropertyValue(13) mustNotEqual 14 // Int

      LongPropertyValue(13) mustNotEqual 13.0
      LongPropertyValue(13) mustNotEqual "13"
    }

    "equal objects with same contents and its contents: Double" in {
      DoublePropertyValue(13.0) mustEqual DoublePropertyValue(13.0)
      DoublePropertyValue(13.0) mustNotEqual DoublePropertyValue(14.0)
      DoublePropertyValue(13.0) mustEqual 13.0
      DoublePropertyValue(13.0) mustNotEqual 14.0

      DoublePropertyValue(13.0) mustNotEqual "13"
    }

    "equal objects with same contents and its contents: String" in {
      StringPropertyValue("13") mustEqual StringPropertyValue("13")
      StringPropertyValue("13") mustNotEqual StringPropertyValue("14")
      StringPropertyValue("13") mustEqual "13"
      StringPropertyValue("13") mustNotEqual "14"

      StringPropertyValue("13") mustNotEqual 13
      StringPropertyValue("13") mustNotEqual 13.0
    }

    "equal objects with same contents and its contents: Boolean" in {
      BooleanPropertyValue(true) mustEqual BooleanPropertyValue(true)
      BooleanPropertyValue(true) mustNotEqual BooleanPropertyValue(false)
      BooleanPropertyValue(true) mustEqual true
      BooleanPropertyValue(true) mustNotEqual false

      BooleanPropertyValue(true) mustNotEqual 13
      BooleanPropertyValue(true) mustNotEqual 13.0
    }

    "equal objects with same contents and its contents: Array" in {
      ArrayPropertyValue(List(1)) mustEqual ArrayPropertyValue(List(1))
      ArrayPropertyValue(List(1)) mustNotEqual ArrayPropertyValue(List(2))
      ArrayPropertyValue(List(1)) mustEqual List(1)
      ArrayPropertyValue(List(1)) mustNotEqual List(2)

      ArrayPropertyValue(List(1)) mustNotEqual 13
      ArrayPropertyValue(List(1)) mustNotEqual 13.0
    }
    
    "equal key" in {
      PropertyKey("13") mustEqual PropertyKey("13")
      PropertyKey("13") mustNotEqual PropertyKey("14")
      PropertyKey("13") mustEqual "13"
      PropertyKey("13") mustNotEqual "14"

      PropertyKey("13") mustNotEqual 13
      PropertyKey("13") mustNotEqual 13.0
    }

    "hashcodes" in todo
  }
}
