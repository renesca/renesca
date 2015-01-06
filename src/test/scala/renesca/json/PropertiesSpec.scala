package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ValueProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class PropertiesSpec extends Specification with Mockito {
  "Properties" can {
    "have a string value" in {
      val jsonAst = """{"key":"value"}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> StringPropertyValue("value"))
      properties.toJson mustEqual jsonAst
    }

    "have a boolean value" in {
      val jsonAst = """{"key":true}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> BooleanPropertyValue(true))
      properties.toJson mustEqual jsonAst
    }

    "have a double value" in {
      val jsonAst = """{"key":17.44}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> DoublePropertyValue(17.44))
      properties.toJson mustEqual jsonAst
    }

    "have a long value" in {
      val jsonAst = """{"key":1744}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> LongPropertyValue(1744))
      properties.toJson mustEqual jsonAst
    }

    "have a null value" in {
      val jsonAst = """{"key":null}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> NullPropertyValue)
      properties.toJson mustEqual jsonAst
    }

    "have a diffent value types" in {
      val jsonAst = """{"key":1744, "key2" : "bier"}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> LongPropertyValue(1744), "key2" -> StringPropertyValue("bier"))
      properties.toJson mustEqual jsonAst
    }

    "have arrays of longs" in {
      val jsonAst = """{"key":[1744, 1516]}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> ArrayPropertyValue(List(LongPropertyValue(1744), LongPropertyValue(1516))))
      properties.toJson mustEqual jsonAst
    }

    "have arrays of doubles" in {
      val jsonAst = """{"key":[17.44, 15.16]}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> ArrayPropertyValue(List(DoublePropertyValue(17.44), DoublePropertyValue(15.16))))
      properties.toJson mustEqual jsonAst
    }

    "have arrays of strings" in {
      val jsonAst = """{"key":["17.44", "15.16"]}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> ArrayPropertyValue(List(StringPropertyValue("17.44"), StringPropertyValue("15.16"))))
      properties.toJson mustEqual jsonAst
    }

    "have arrays of booleans" in {
      val jsonAst = """{"key":[true, false, true]}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> ArrayPropertyValue(List(BooleanPropertyValue(true), BooleanPropertyValue(false), BooleanPropertyValue(true))))
      properties.toJson mustEqual jsonAst
    }

    "have arrays of null" in {
      val jsonAst = """{"key":[null, null]}""".parseJson
      val properties = jsonAst.convertTo[Map[String, PropertyValue]]

      properties mustEqual Map("key" -> ArrayPropertyValue(List(NullPropertyValue, NullPropertyValue)))
      properties.toJson mustEqual jsonAst
    }
  }
}
