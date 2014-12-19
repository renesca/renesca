package renesca



trait PropertyValue
case class LongPropertyValue(value:Long) extends PropertyValue
case class DoublePropertyValue(value:Double) extends PropertyValue
case class StringPropertyValue(value:String) extends PropertyValue
case class BooleanPropertyValue(value:Boolean) extends PropertyValue
case class ArrayLongPropertyValue(value:Seq[Long]) extends PropertyValue
case class ArrayDoublePropertyValue(value:Seq[Double]) extends PropertyValue
case class ArrayStringPropertyValue(value:Seq[String]) extends PropertyValue
case class ArrayBooleanPropertyValue(value:Seq[Boolean]) extends PropertyValue

object PropertyValue {
  implicit def primitiveToPropertyValue(x:Long):PropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Double):PropertyValue = DoublePropertyValue(x)
  implicit def primitiveToPropertyValue(x:String):PropertyValue = StringPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Boolean):PropertyValue = BooleanPropertyValue(x)

  implicit def PropertyValueToPrimitive(x:LongPropertyValue):Long = x.value
  implicit def PropertyValueToPrimitive(x:DoublePropertyValue):Double = x.value
  implicit def PropertyValueToPrimitive(x:StringPropertyValue):String = x.value
  implicit def PropertyValueToPrimitive(x:BooleanPropertyValue):Boolean = x.value

  implicit def SeqLongToPropertyValue(x:Seq[Long]):PropertyValue = ArrayLongPropertyValue(x)
  implicit def SeqDoubleToPropertyValue(x:Seq[Double]):PropertyValue = ArrayDoublePropertyValue(x)
  implicit def SeqStringToPropertyValue(x:Seq[String]):PropertyValue = ArrayStringPropertyValue(x)
  implicit def SeqBooleanToPropertyValue(x:Seq[Boolean]):PropertyValue = ArrayBooleanPropertyValue(x)

  //TODO: ArrayPropertyValue to Array
}


