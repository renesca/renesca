package object renesca {

  import collection.mutable
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
  import collection.breakOut

  type RelationType = NonBacktickName
  val RelationType = NonBacktickName
  type Label = NonBacktickName
  val Label = NonBacktickName
  type PropertyKey = NonBacktickName
  val PropertyKey = NonBacktickName
  type ParameterKey = NonBacktickName
  val ParameterKey = NonBacktickName

  class NonBacktickName private(val name: String) extends AnyVal
  object NonBacktickName {
    implicit def StringToNonBacktickName(name: String): NonBacktickName = apply(name)
    def apply(s: String): NonBacktickName = {
      require(!s.contains("`"), "Backticks are not allowed in ... names")
      new NonBacktickName(s)
    }
  }

  implicit val nonBacktickEncoder: Encoder[NonBacktickName] = Encoder.encodeString.contramap[NonBacktickName](nbn => nbn.name)
  implicit val nonBacktickDecoder: Decoder[NonBacktickName] = Decoder.decodeString.emap(str => Right(NonBacktickName(str)))

  implicit val nonBacktickKeyEncoder = new KeyEncoder[NonBacktickName] {
    override def apply(nbn: NonBacktickName): String = nbn.name
  }
  implicit val nonBacktickKeyDecoder = new KeyDecoder[NonBacktickName] {
    override def apply(str: String): Option[NonBacktickName] = Some(NonBacktickName(str))
  }

  type ParameterValue = Json
  type ParameterMap = Map[ParameterKey, ParameterValue]

  type PropertyValue = Json
  type PropertyMap = Map[PropertyKey, PropertyValue]
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]

  object ParameterMap {
    def apply(tuples: (String, ParameterValue)*): ParameterMap = {
      tuples.map { case (k,v) => ParameterKey(k) -> v }(breakOut[Seq[(String,ParameterValue)], (ParameterKey, ParameterValue), Map[ParameterKey, ParameterValue]])
    }
    def empty = Map.empty[ParameterKey, ParameterValue]
  }
  val PropertyMap = ParameterMap
}
