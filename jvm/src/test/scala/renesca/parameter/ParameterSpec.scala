package renesca.parameter

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.parameter.implicits._

@RunWith(classOf[JUnitRunner])
class ParameterSpec extends Specification {
  "Property" should {
    "not equal objects of different type" in {
      LongPropertyValue(1) mustNotEqual DoublePropertyValue(1)
      LongPropertyValue(1) mustNotEqual StringPropertyValue("1")
      LongPropertyValue(1) mustNotEqual BooleanPropertyValue(true)
      LongPropertyValue(1) mustNotEqual LongArrayPropertyValue(1)

      DoublePropertyValue(1) mustNotEqual StringPropertyValue("1")
      DoublePropertyValue(1) mustNotEqual BooleanPropertyValue(true)
      DoublePropertyValue(1) mustNotEqual LongArrayPropertyValue(1)

      StringPropertyValue("1") mustNotEqual BooleanPropertyValue(true)
      StringPropertyValue("1") mustNotEqual LongArrayPropertyValue(1)

      BooleanPropertyValue(true) mustNotEqual LongArrayPropertyValue(1)
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
      LongArrayPropertyValue(1) mustEqual LongArrayPropertyValue(1)
      LongArrayPropertyValue(1) mustNotEqual LongArrayPropertyValue(2)
      LongArrayPropertyValue(1) mustEqual List(1)
      LongArrayPropertyValue(1) mustNotEqual List(2)

      LongArrayPropertyValue(1) mustNotEqual 13
      LongArrayPropertyValue(1) mustNotEqual 13.0
    }

    "have equal on keys" in {
      PropertyKey("13") mustEqual PropertyKey("13")
      PropertyKey("13") mustNotEqual PropertyKey("14")
      PropertyKey("13") mustEqual "13"
      PropertyKey("13") mustNotEqual "14"

      PropertyKey("13") mustNotEqual 13
      PropertyKey("13") mustNotEqual 13.0
    }

    "hashcodes" in todo
  }

  "Parameter" should {
    "equal objects with same contents and its contents: Array" in {
      ArrayParameterValue(List(1)) mustEqual ArrayParameterValue(List(1))
      ArrayParameterValue(List(1)) mustNotEqual ArrayParameterValue(List(2))
      ArrayParameterValue(List(1)) mustEqual List(1)
      ArrayParameterValue(List(1)) mustNotEqual List(2)

      ArrayParameterValue(List(1)) mustNotEqual 13
      ArrayParameterValue(List(1)) mustNotEqual 13.0
    }

    "equal objects with same contents and its contents: Map" in {
      MapParameterValue(Map("k" -> 1)) mustEqual MapParameterValue(Map("k" -> 1))
      MapParameterValue(Map("k" -> 1)) mustNotEqual MapParameterValue(Map("k" -> 2))
      MapParameterValue(Map("k" -> 1)) mustNotEqual MapParameterValue(Map("x" -> 1))
      MapParameterValue(Map("k" -> 1)) mustEqual Map(("k", 1))
      MapParameterValue(Map("k" -> 1)) mustNotEqual Map(("k", 2))

      MapParameterValue(Map("k" -> 1)) mustNotEqual 13
      MapParameterValue(Map("k" -> 1)) mustNotEqual 13.0
    }

    "equal objects with same contents and its contents: nested Maps and Arrays" in {
      MapParameterValue(Map("a" -> ArrayParameterValue(List("a", "b")), "b" -> 5)) mustEqual MapParameterValue(Map("a" -> ArrayParameterValue(List("a", "b")), "b" -> 5))
      MapParameterValue(Map("a" -> ArrayParameterValue(List("a", "b")), "b" -> 5)) mustNotEqual MapParameterValue(Map("a" -> ArrayParameterValue(List("a", "x")), "b" -> 5))
      MapParameterValue(Map("a" -> ArrayParameterValue(List("a", "b")), "b" -> 5)) mustEqual Map(("a", List("a", "b")), ("b", 5))
      MapParameterValue(Map("a" -> ArrayParameterValue(List("a", "b")), "b" -> 5)) mustNotEqual Map(("a", List("a", "x")), ("b", 5))

      ArrayParameterValue(List(MapParameterValue(Map("a" -> 7)))) mustEqual ArrayParameterValue(List(MapParameterValue(Map("a" -> 7))))
      ArrayParameterValue(List(MapParameterValue(Map("a" -> 7)))) mustNotEqual ArrayParameterValue(List(MapParameterValue(Map("a" -> 8))))
      ArrayParameterValue(List(MapParameterValue(Map("a" -> 7)))) mustEqual List(Map(("a", 7)))
      ArrayParameterValue(List(MapParameterValue(Map("a" -> 7)))) mustNotEqual List(Map(("a", 8)))
    }

    "hashcodes" in todo

    "Syntactic sugar for casting" in {
      "asString" in { val p: ParameterValue = StringPropertyValue("Kleiderschrank"); p.asString mustEqual "Kleiderschrank" }
      "asLong" in { val p: ParameterValue = LongPropertyValue(5); p.asLong mustEqual 5 }
      "asDouble" in { val p: ParameterValue = DoublePropertyValue(5.5); p.asDouble mustEqual 5.5 }
      "asBoolean" in { val p: ParameterValue = BooleanPropertyValue(false); p.asBoolean mustEqual false }

      "asStringArray" in { val p: ParameterValue = StringArrayPropertyValue("Kleiderschrank"); p.asStringArray mustEqual Seq("Kleiderschrank") }
      "asLongArray" in { val p: ParameterValue = LongArrayPropertyValue(5); p.asLongArray mustEqual Seq(5) }
      "asDoubleArray" in { val p: ParameterValue = DoubleArrayPropertyValue(5.5); p.asDoubleArray mustEqual Seq(5.5) }
      "asBooleanArray" in { val p: ParameterValue = BooleanArrayPropertyValue(false); p.asBooleanArray mustEqual Seq(false) }

      "asMap" in { val p: ParameterValue = MapParameterValue(Map("A" -> 3)); p.asMap("A") mustEqual 3 }
    }
  }
}
