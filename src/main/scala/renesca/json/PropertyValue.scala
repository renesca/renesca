package renesca.json

import scala.collection.immutable

sealed trait ParameterValue
sealed trait PropertyValue extends ParameterValue

case class LongPropertyValue(value:Long) extends PropertyValue
case class DoublePropertyValue(value:Double) extends PropertyValue
case class StringPropertyValue(value:String) extends PropertyValue
case class BooleanPropertyValue(value:Boolean) extends PropertyValue
case object NullPropertyValue extends PropertyValue
case class ArrayPropertyValue(value:Seq[PropertyValue]) extends PropertyValue

case class ArrayParameterValue(value:Seq[ParameterValue]) extends ParameterValue
case class MapParameterValue(value:Map[String,ParameterValue]) extends ParameterValue


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

  //TODO: ArrayPropertyValue to Array
}

object ParameterValue {
  implicit def MapToParameterValue(map:Map[String, ParameterValue]):ParameterValue = MapParameterValue(map)
  def Map(kvs:(String,ParameterValue)*):Map[String, ParameterValue] = immutable.Map(kvs:_*)


  implicit def MapParameterValueToMap(map:MapParameterValue):Map[String,ParameterValue] = map.value
}


