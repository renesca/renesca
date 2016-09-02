package renesca.json.protocols

import renesca.parameter._
import spray.json._

object ParameterProtocol extends DefaultJsonProtocol {

  implicit object JsonPropertyKeyFormat extends RootJsonFormat[PropertyKey] {
    override def write(pv: PropertyKey) = JsString(pv.name)

    override def read(value: JsValue): PropertyKey = value match {
      case JsString(str) => PropertyKey(str)
      case json          => deserializationError(s"Can not deserialize property key of type $json")
    }
  }

  implicit object JsonPrimitivePropertyValueFormat extends RootJsonFormat[PrimitivePropertyValue] {
    override def write(pv: PrimitivePropertyValue) = pv match {
      case LongPropertyValue(value)    => JsNumber(value)
      case DoublePropertyValue(value)  => JsNumber(value)
      case StringPropertyValue(value)  => JsString(value)
      case BooleanPropertyValue(value) => JsBoolean(value)
      case NullPropertyValue           => JsNull
    }

    override def read(value: JsValue) = value match {
      case JsString(str)                     => StringPropertyValue(str)
      case JsNumber(num) if num.isValidLong  => LongPropertyValue(num.toLong)
      case JsNumber(num) if !num.isValidLong => DoublePropertyValue(num.toDouble)
      case JsBoolean(bool)                   => BooleanPropertyValue(bool)
      case JsNull                            => NullPropertyValue
      case json                              => deserializationError(s"Can not deserialize property value of type $json")
    }
  }

  implicit object JsonPropertyValueFormat extends RootJsonFormat[PropertyValue] {
    override def write(pv: PropertyValue) = pv match {
      case x: PrimitivePropertyValue => x.toJson
      case apv: ArrayPropertyValue   => apv.elements.toJson
    }

    override def read(value: JsValue) = value match {
      case x@(_: JsString | _: JsNumber | _: JsBoolean | JsNull) => x.convertTo[PrimitivePropertyValue]
      case jsArray: JsArray                                      =>
        try {
          val elements = jsArray.convertTo[Seq[PrimitivePropertyValue]]
          elements.head match {
            case x: LongPropertyValue    => LongArrayPropertyValue(elements.asInstanceOf[Seq[LongPropertyValue]]: _*)
            case x: DoublePropertyValue  => DoubleArrayPropertyValue(elements.asInstanceOf[Seq[DoublePropertyValue]]: _*)
            case x: StringPropertyValue  => StringArrayPropertyValue(elements.asInstanceOf[Seq[StringPropertyValue]]: _*)
            case x: BooleanPropertyValue => BooleanArrayPropertyValue(elements.asInstanceOf[Seq[BooleanPropertyValue]]: _*)
            case NullPropertyValue       => deserializationError(s"Can not deserialize null-arrays: $jsArray")
          }
        } catch {
          case e: ClassCastException =>
            deserializationError(s"Can not deserialize property array of type $jsArray")
        }
      case json                                                  => deserializationError(s"Can not deserialize property value of type $json")
    }
  }

  implicit object JsonArrayParameterValueFormat extends RootJsonFormat[ArrayParameterValue] {

    override def write(apv: ArrayParameterValue) = apv match {
      case ArrayParameterValue(elements) => JsArray(elements map (_.toJson): _*)
    }

    override def read(value: JsValue) = value match {
      case JsArray(elements) => ArrayParameterValue(elements.toArray map (_.convertTo[ParameterValue]))
      case json              => deserializationError(s"Can not deserialize ArrayParameterValue from: $json")
    }
  }

  implicit object JsonParameterValueFormat extends RootJsonFormat[ParameterValue] {

    // Parameters can be arbitrarily nested maps and arrays
    // therefore we need to call write and read recursively
    override def write(pv: ParameterValue) = pv match {
      case pv: PropertyValue                        => pv.toJson
      case arrayParameterValue: ArrayParameterValue => arrayParameterValue.toJson
      case MapParameterValue(keyValuePairs)         => JsObject(keyValuePairs.map { case (key, value) => (key.name, write(value)) })
    }

    override def read(jsvalue: JsValue) = jsvalue match {
      case x@(_: JsString | _: JsNumber | _: JsBoolean | JsNull) => x.convertTo[PropertyValue]
      case jsArray: JsArray                                      => jsArray.convertTo[ArrayParameterValue]
      case JsObject(keyValuePairs)                               => MapParameterValue(keyValuePairs.map { case (key, value) => (PropertyKey(key), read(value)) })
    }
  }

}
