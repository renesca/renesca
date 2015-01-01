package renesca.json.protocols

import renesca.json._
import spray.json.{DefaultJsonProtocol, JsArray, JsBoolean, JsNumber, JsObject, JsString, JsValue, RootJsonFormat, deserializationError}
import renesca.compat

object PropertyValueProtocol extends DefaultJsonProtocol {

  implicit object PropertyJsonValueFormat extends RootJsonFormat[PropertyValue] {

    override def write(p: PropertyValue) = ???

    override def read(value: JsValue) = value match {
      case JsString(str) => StringPropertyValue(str)
      case JsNumber(num) if num.isValidLong => LongPropertyValue(num.toLong)
      case JsNumber(num) if compat.bigDecimal_isDecimalDouble(num) => DoublePropertyValue(num.toDouble)
      case JsBoolean(bool) => BooleanPropertyValue(bool)
      case JsArray(arr) => ArrayPropertyValue(arr.map(read)) // TODO: check type of arr
      case json => deserializationError(s"can not deserialize property value of type $json")
    }
  }

  implicit object PropertiesJsonValueFormat extends RootJsonFormat[Map[String, PropertyValue]] {

    override def write(p: Map[String, PropertyValue]) = ???

    override def read(value: JsValue) = value match {
      case JsObject(fields) => {
        fields.map { case (k, v) => k -> PropertyJsonValueFormat.read(v)}.toMap
      }
      case json => deserializationError(s"can not deserialize property value map of type $json")
    }
  }

}
