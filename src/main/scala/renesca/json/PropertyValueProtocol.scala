package renesca.json

import renesca.graph.BooleanPropertyValue
import renesca.graph.PropertyValue
import renesca.graph.StringPropertyValue
import spray.json.DefaultJsonProtocol
import spray.json.JsBoolean
import spray.json.JsString
import spray.json.JsValue
import spray.json.RootJsonFormat
import spray.json.deserializationError
import spray.json.JsObject
import renesca.graph.LongPropertyValue
import renesca.graph.DoublePropertyValue
import spray.json.JsNumber
import spray.json.JsArray
import renesca.graph.ArrayPropertyValue

object PropertyValueProtocol extends DefaultJsonProtocol {

  implicit object PropertyJsonValueFormat extends RootJsonFormat[PropertyValue] {
    
      override def write(p:PropertyValue) = ???
    
  	  override def read(value : JsValue) = value match {
  	    case JsString(str) => StringPropertyValue(str)
  	    case JsNumber(num) if num.isValidLong => LongPropertyValue(num.toLong)
  	    case JsNumber(num) if num.isDecimalDouble => DoublePropertyValue(num.toDouble)
  	    case JsBoolean(bool) => BooleanPropertyValue(bool)
        case JsArray(arr) => ArrayPropertyValue(arr.map(read))
  	    case json => deserializationError(s"can not deserialize property value of type $json")
  	  }
  }

  implicit object PropertiesJsonValueFormat extends RootJsonFormat[Map[String, PropertyValue]] {
	  
	  override def write(p:Map[String,PropertyValue]) = ???
	  
	  override def read(value : JsValue) = value match {
			  case JsObject(fields) => {
			    fields.map{case (k,v) => k -> PropertyJsonValueFormat.read(v)}.toMap
			  }
			  case json => deserializationError(s"can not deserialize property value map of type $json")
	  }
  }
}