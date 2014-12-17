package renesca



trait PropertyValue
case class LongPropertyValue(x:Long) extends PropertyValue
case class DoublePropertyValue(x:Double) extends PropertyValue
case class StringPropertyValue(x:String) extends PropertyValue
case class BooleanPropertyValue(x:Boolean) extends PropertyValue
case class ArrayLongPropertyValue(x:Seq[Long]) extends PropertyValue
case class ArrayDoublePropertyValue(x:Seq[Double]) extends PropertyValue
case class ArrayStringPropertyValue(x:Seq[String]) extends PropertyValue
case class ArrayBooleanPropertyValue(x:Seq[Boolean]) extends PropertyValue

object PropertyValue {
  implicit def primitiveToPropertyValue(x:Long):PropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Double):PropertyValue = DoublePropertyValue(x)
  implicit def primitiveToPropertyValue(x:String):PropertyValue = StringPropertyValue(x)
  implicit def primitiveToPropertyValue(x:Boolean):PropertyValue = BooleanPropertyValue(x)

  implicit def SeqLongToPropertyValue(x:Seq[Long]):PropertyValue = ArrayLongPropertyValue(x)
  implicit def SeqDoubleToPropertyValue(x:Seq[Double]):PropertyValue = ArrayDoublePropertyValue(x)
  implicit def SeqStringToPropertyValue(x:Seq[String]):PropertyValue = ArrayStringPropertyValue(x)
  implicit def SeqBooleanToPropertyValue(x:Seq[Boolean]):PropertyValue = ArrayBooleanPropertyValue(x)
}


