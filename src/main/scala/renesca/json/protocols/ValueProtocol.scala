package renesca.json.protocols

import renesca.compat
import renesca.json._
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, deserializationError, serializationError}

import scala.collection.mutable

object ValueProtocol extends DefaultJsonProtocol {

  implicit object JsonValueFormat extends RootJsonFormat[PropertyValue] {

    override def write(p: PropertyValue) = ???

    override def read(value: JsValue) = value match {
      case JsString(str) => StringPropertyValue(str)
      case JsNumber(num) if num.isValidLong => LongPropertyValue(num.toLong)
      case JsNumber(num) if compat.bigDecimal_isDecimalDouble(num) => DoublePropertyValue(num.toDouble) //TODO: !num.isValidLong ?
      case JsBoolean(bool) => BooleanPropertyValue(bool)
      case JsArray(arr) => ArrayPropertyValue(arr.map(read)) // TODO: check type of arr
      case json => deserializationError(s"can not deserialize property value of type $json")
    }
  }

  implicit object PropertiesJsonValueFormat extends RootJsonFormat[Map[String, PropertyValue]] {

    override def write(jsonValues: Map[String, PropertyValue]) = {
      val jsonMap = jsonValues.map { case (k, v) => k -> toJsValue(v)}.toMap
      JsObject(jsonMap)
    }

    def toJsValue(value : PropertyValue) : JsValue =  value match {
      case StringPropertyValue(str) => JsString(str)
      case DoublePropertyValue(num) => JsNumber(num)
      case LongPropertyValue(num) => JsNumber(num)
      case ArrayPropertyValue(arr) => JsArray((arr map toJsValue).toVector)
      case other => serializationError(s"can not serialize value of type $other")
    }

    override def read(value: JsValue) = value match {
      case JsObject(fields) =>
        fields.map { case (k, v) => k -> JsonValueFormat.read(v)}.toMap
      case json => deserializationError(s"can not deserialize property value map of type $json")
    }
  }

}
