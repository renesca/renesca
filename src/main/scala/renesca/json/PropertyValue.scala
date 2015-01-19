package renesca.json

import renesca.NonBacktickName

import scala.collection.immutable

case class PropertyKey(name:String) extends NonBacktickName

sealed trait ParameterValue
sealed trait SoleParameterValue extends ParameterValue
sealed trait PropertyValue extends ParameterValue

case class LongPropertyValue(value:Long) extends PropertyValue
case class DoublePropertyValue(value:Double) extends PropertyValue
case class StringPropertyValue(value:String) extends PropertyValue
case class BooleanPropertyValue(value:Boolean) extends PropertyValue
case class ArrayPropertyValue(value:Seq[PropertyValue]) extends PropertyValue {
  //TODO: forbid nesting of propertyvalues
}

case class ArrayParameterValue(value:Seq[ParameterValue]) extends SoleParameterValue
case class MapParameterValue(value:Map[PropertyKey,ParameterValue]) extends SoleParameterValue

object PropertyValue {
  implicit def primitiveToPropertyValue(x: Long): PropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x: Int): PropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x: Double): PropertyValue = DoublePropertyValue(x)
  implicit def primitiveToPropertyValue(x: String): PropertyValue = StringPropertyValue(x)
  implicit def primitiveToPropertyValue(x: Boolean): PropertyValue = BooleanPropertyValue(x)

  implicit def SeqLongToPropertyValue(xs: Seq[Long]): PropertyValue = ArrayPropertyValue(xs map LongPropertyValue)
  implicit def SeqIntToPropertyValue(xs: Seq[Int]): PropertyValue = ArrayPropertyValue(xs.map(x => LongPropertyValue(x.toLong)))
  implicit def SeqDoubleToPropertyValue(xs: Seq[Double]): PropertyValue = ArrayPropertyValue(xs map DoublePropertyValue)
  implicit def SeqStringToPropertyValue(xs: Seq[String]): PropertyValue = ArrayPropertyValue(xs map StringPropertyValue)
  implicit def SeqBooleanToPropertyValue(xs: Seq[Boolean]): PropertyValue = ArrayPropertyValue(xs map BooleanPropertyValue)

  implicit def propertyValueToPrimitive(x:LongPropertyValue):Long = x.value
  implicit def propertyValueToPrimitive(x:DoublePropertyValue):Double = x.value
  implicit def propertyValueToPrimitive(x:StringPropertyValue):String = x.value
  implicit def propertyValueToPrimitive(x:BooleanPropertyValue):Boolean = x.value

  implicit def primitiveToPropertyKey(x:String):PropertyKey = PropertyKey(x)
  implicit def propertyKeyToPrimitive(x:PropertyKey):String = x.name

  //TODO: ArrayPropertyValue to Array
}

object ParameterValue {
  implicit def PropertyKeyMapToMapParameterValue(map:Map[PropertyKey, ParameterValue]):MapParameterValue = MapParameterValue(map)

  implicit def MapParameterValueToPropertyKeyMap(map:MapParameterValue):Map[PropertyKey,ParameterValue] = map.value

  implicit def StringMapToPropertyKeyMap(key: String)= new AnyRef {
    def ->(x: PropertyValue) = (PropertyKey(key),x)
    def ->(x: SoleParameterValue) = (PropertyKey(key),x)
  }
}


