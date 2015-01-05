package renesca.json

trait Value
case class LongValue(value:Long) extends Value
case class DoubleValue(value:Double) extends Value
case class StringValue(value:String) extends Value
case class BooleanValue(value:Boolean) extends Value
case class ArrayValue(value:Seq[Value]) extends Value

object Value {
  implicit def primitiveToValue(x:Long):Value = LongValue(x)
  implicit def primitiveToValue(x:Double):Value = DoubleValue(x)
  implicit def primitiveToValue(x:String):Value = StringValue(x)
  implicit def primitiveToValue(x:Boolean):Value = BooleanValue(x)

  implicit def valueToPrimitive(x:LongValue):Long = x.value
  implicit def valueToPrimitive(x:DoubleValue):Double = x.value
  implicit def valueToPrimitive(x:StringValue):String = x.value
  implicit def valueToPrimitive(x:BooleanValue):Boolean = x.value

  implicit def SeqLongToValue(x:Seq[Long]):Value = ArrayValue(x map LongValue)
  implicit def SeqDoubleToValue(x:Seq[Double]):Value = ArrayValue(x map DoubleValue)
  implicit def SeqStringToValue(x:Seq[String]):Value = ArrayValue(x map StringValue)
  implicit def SeqBooleanToValue(x:Seq[Boolean]):Value = ArrayValue(x map BooleanValue)

  //TODO: ArrayPropertyValue to Array
}


