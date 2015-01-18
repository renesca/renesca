package renesca.json.protocols

import renesca.json._
import spray.json._

object ValueProtocol extends DefaultJsonProtocol {

  implicit object JsonPropertyValueFormat extends RootJsonFormat[PropertyValue] {
    override def write(pv: PropertyValue) = pv match {
      case LongPropertyValue(value) => JsNumber(value)
      case DoublePropertyValue(value) => JsNumber(value)
      case StringPropertyValue(value) => JsString(value)
      case BooleanPropertyValue(value) => JsBoolean(value)
      case ArrayPropertyValue(value) => JsArray((value map write).toVector)
    }

    override def read(value: JsValue) = value match {
      case JsString(str) => StringPropertyValue(str)
      case JsNumber(num) if num.isValidLong => LongPropertyValue(num.toLong)
      case JsNumber(num) if !num.isValidLong => DoublePropertyValue(num.toDouble)
      case JsBoolean(bool) => BooleanPropertyValue(bool)
      case JsArray(arr) => ArrayPropertyValue(arr.map(read))
      case json => deserializationError(s"can not deserialize property value of type $json")
    }
  }

  implicit object JsonParameterValueFormat extends RootJsonFormat[ParameterValue] {
    override def write(pv: ParameterValue) = pv match {
      case pv:PropertyValue => JsonPropertyValueFormat.write(pv)
      case ArrayParameterValue(seq) => JsArray((seq map write).toVector)
      case MapParameterValue(map) => JsObject(map mapValues write)
    }

    override def read(value: JsValue) = value match {
      case x@(_:JsString | _:JsNumber | _:JsBoolean | JsNull) => JsonPropertyValueFormat.read(x)
      case JsArray(array) => ArrayParameterValue(array map read)
      case JsObject(map) => MapParameterValue(map mapValues read)
    }
  }
}
