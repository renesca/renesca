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
      val json = """
        {"key":"value"}
        """
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> StringValue("value"))
    }

    "have a boolean value" in {
      val json = """
        {"key":true}
        """
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> BooleanValue(true))
    }

    "have a double value" in {
      val json = """
    			 {"key":17.44}
    			 """
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> DoubleValue(17.44))
    }

    "have a long value" in {
      val json = """
    			 {"key":1744}
    			 """
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> LongValue(1744))
    }

    "have a diffent value types" in {
      val json = """
    			{"key":1744, "key2" : "bier"}
    			"""
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> LongValue(1744), "key2" -> StringValue("bier"))
    }

    "have arrays of longs" in {
      val json = """
    			{"key":[1744, 1516]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> ArrayValue(List(LongValue(1744), LongValue(1516))))
    }

    "have arrays of doubles" in {
      val json = """
    			{"key":[17.44, 15.16]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> ArrayValue(List(DoubleValue(17.44), DoubleValue(15.16))))
    }

    "have arrays of strings" in {
      val json = """
    			{"key":["17.44", "15.16"]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> ArrayValue(List(StringValue("17.44"), StringValue("15.16"))))
    }

    "have arrays of booleans" in {
      val json = """
    			{"key":[true, false, true]}
    			"""
      val properties = json.parseJson.convertTo[Map[String, Value]]
      properties mustEqual Map("key" -> ArrayValue(List(BooleanValue(true), BooleanValue(false), BooleanValue(true))))
    }

    "not have arrays of different types" in {
      val json = """
    			{"key":[true, "bier", true]}
    			"""
      json.parseJson.convertTo[Map[String, Value]] must throwA[IllegalArgumentException]
    }.pendingUntilFixed
  }
}
