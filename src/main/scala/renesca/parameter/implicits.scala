package renesca.parameter

object implicits {
  implicit def primitiveToPropertyValue(x: Long): LongPropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x: Int): LongPropertyValue = LongPropertyValue(x)
  implicit def primitiveToPropertyValue(x: Double): DoublePropertyValue = DoublePropertyValue(x)
  implicit def primitiveToPropertyValue(x: String): StringPropertyValue = StringPropertyValue(x)
  implicit def primitiveToPropertyValue(x: Boolean): BooleanPropertyValue = BooleanPropertyValue(x)

  implicit def SeqLongToPropertyValue(xs: Seq[Long]): LongArrayPropertyValue = LongArrayPropertyValue(xs map LongPropertyValue: _*)
  implicit def SeqIntToPropertyValue(xs: Seq[Int]): LongArrayPropertyValue = LongArrayPropertyValue(xs map (e => LongPropertyValue(e.toLong)): _*)
  implicit def SeqDoubleToPropertyValue(xs: Seq[Double]): DoubleArrayPropertyValue = DoubleArrayPropertyValue(xs map DoublePropertyValue: _*)
  implicit def SeqStringToPropertyValue(xs: Seq[String]): StringArrayPropertyValue = StringArrayPropertyValue(xs map StringPropertyValue: _*)
  implicit def SeqBooleanToPropertyValue(xs: Seq[Boolean]): BooleanArrayPropertyValue = BooleanArrayPropertyValue(xs map BooleanPropertyValue: _*)


  implicit def propertyValueToPrimitive(x: LongPropertyValue): Long = x.value
  implicit def propertyValueToPrimitive(x: DoublePropertyValue): Double = x.value
  implicit def propertyValueToPrimitive(x: StringPropertyValue): String = x.value
  implicit def propertyValueToPrimitive(x: BooleanPropertyValue): Boolean = x.value

  implicit def LongArrayPropertyValueToSeq(x: LongArrayPropertyValue): Seq[Long] = x.elements map (_.value)
  implicit def DoubleArrayPropertyValueToSeq(x: DoubleArrayPropertyValue): Seq[Double] = x.elements map (_.value)
  implicit def StringArrayPropertyValueToSeq(x: StringArrayPropertyValue): Seq[String] = x.elements map (_.value)
  implicit def BooleanArrayPropertyValueToSeq(x: BooleanArrayPropertyValue): Seq[Boolean] = x.elements map (_.value)

  implicit def primitiveToPropertyKey(x: String): PropertyKey = PropertyKey(x)
  implicit def propertyKeyToPrimitive(x: PropertyKey): String = x.name

  implicit def StringMapToPropertyKeyMap(key: String) = new AnyRef {
    def ->(x: PropertyValue) = (PropertyKey(key), x)
    def ->(x: SoleParameterValue) = (PropertyKey(key), x)
  }

  implicit def ParameterMapToMapParameterValue(map: ParameterMap): MapParameterValue = MapParameterValue(map)
  implicit def MapParameterValueToParameterMap(map: MapParameterValue): ParameterMap = map.value
}
