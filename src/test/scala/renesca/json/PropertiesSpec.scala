package renesca.json

import org.junit.runner.RunWith
import protocols.PropertyValueProtocol._
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import spray.json._
import renesca.graph.PropertyValue
import renesca.graph.{ StringPropertyValue, BooleanPropertyValue }
import spray.json.DefaultJsonProtocol._
import renesca.graph.DoublePropertyValue
import renesca.graph.LongPropertyValue
import renesca.graph.ArrayPropertyValue
import renesca.json.protocols.PropertyValueProtocol

@RunWith(classOf[JUnitRunner])
class PropertiesSpec extends Specification with Mockito {
  "Properties" can {
    "have a string value" in {
      val json = """
        {"key":"value"}
        """
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> StringPropertyValue("value"))
    }

    "have a boolean value" in {
      val json = """
        {"key":true}
        """
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> BooleanPropertyValue(true))
    }

    "have a double value" in {
      val json = """
    			 {"key":17.44}
    			 """
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> DoublePropertyValue(17.44))
    }

    "have a long value" in {
      val json = """
    			 {"key":1744}
    			 """
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> LongPropertyValue(1744))
    }

    "have a diffent value types" in {
      val json = """
    			{"key":1744, "key2" : "bier"}
    			"""
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> LongPropertyValue(1744), "key2" -> StringPropertyValue("bier"))
    }

    "have arrays of longs" in {
      val json = """
    			{"key":[1744, 1516]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> ArrayPropertyValue(List(LongPropertyValue(1744), LongPropertyValue(1516))))
    }

    "have arrays of doubles" in {
      val json = """
    			{"key":[17.44, 15.16]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> ArrayPropertyValue(List(DoublePropertyValue(17.44), DoublePropertyValue(15.16))))
    }

    "have arrays of strings" in {
      val json = """
    			{"key":["17.44", "15.16"]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> ArrayPropertyValue(List(StringPropertyValue("17.44"), StringPropertyValue("15.16"))))
    }

    "have arrays of booleans" in {
      val json = """
    			{"key":[true, false, true]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, PropertyValue]]
      properties mustEqual Map("key" -> ArrayPropertyValue(List(BooleanPropertyValue(true), BooleanPropertyValue(false), BooleanPropertyValue(true))))
    }

    "not have arrays of different types" in {
      val json = """
    			{"key":[true, "bier", true]}
    			"""
      json.parseJson.convertTo[Map[String, PropertyValue]] must throwA[IllegalArgumentException]
    }.pendingUntilFixed
  }
}
