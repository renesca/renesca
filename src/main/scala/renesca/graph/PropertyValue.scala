package renesca.graph

trait PropertyValue
case class LongPropertyValue(value:Long) extends PropertyValue
case class DoublePropertyValue(value:Double) extends PropertyValue
case class StringPropertyValue(value:String) extends PropertyValue
case class BooleanPropertyValue(value:Boolean) extends PropertyValue
case class ArrayPropertyValue(value:Seq[PropertyValue]) extends PropertyValue

object PropertyValue {
  implicit def primitiveToPropertyValue(x:Long):PropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Double):PropertyValue = DoublePropertyValue(x)
  implicit def primitiveToPropertyValue(x:String):PropertyValue = StringPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Boolean):PropertyValue = BooleanPropertyValue(x)

  implicit def PropertyValueToPrimitive(x:LongPropertyValue):Long = x.value
  implicit def PropertyValueToPrimitive(x:DoublePropertyValue):Double = x.value
  implicit def PropertyValueToPrimitive(x:StringPropertyValue):String = x.value
  implicit def PropertyValueToPrimitive(x:BooleanPropertyValue):Boolean = x.value

  implicit def SeqLongToPropertyValue(x:Seq[Long]):PropertyValue = ArrayPropertyValue(x map LongPropertyValue)
  implicit def SeqDoubleToPropertyValue(x:Seq[Double]):PropertyValue = ArrayPropertyValue(x map DoublePropertyValue)
  implicit def SeqStringToPropertyValue(x:Seq[String]):PropertyValue = ArrayPropertyValue(x map StringPropertyValue)
  implicit def SeqBooleanToPropertyValue(x:Seq[Boolean]):PropertyValue = ArrayPropertyValue(x map BooleanPropertyValue)

  //TODO: ArrayPropertyValue to Array
}


