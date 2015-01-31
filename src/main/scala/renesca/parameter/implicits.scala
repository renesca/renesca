package renesca.parameter

object implicits {
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

  implicit def StringMapToPropertyKeyMap(key: String) = new AnyRef {
    def ->(x: PropertyValue) = (PropertyKey(key),x)
    def ->(x: SoleParameterValue) = (PropertyKey(key),x)
  }
  //TODO: ArrayPropertyValue to List

  implicit def ParameterMapToMapParameterValue(map:ParameterMap):MapParameterValue = MapParameterValue(map)
  implicit def MapParameterValueToParameterMap(map:MapParameterValue):ParameterMap = map.value
}
