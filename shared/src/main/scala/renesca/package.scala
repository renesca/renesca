package object renesca {

  import collection.mutable
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  type RelationType = NonBacktickName
  val RelationType = NonBacktickName
  type Label = NonBacktickName
  val Label = NonBacktickName
  type PropertyKey = NonBacktickName
  val PropertyKey = NonBacktickName

  class NonBacktickName private(val name: String) extends AnyVal
  object NonBacktickName {
    implicit def StringToNonBacktickName(name: String): NonBacktickName = apply(name)
    def apply(s: String): NonBacktickName = {
      require(!s.contains("`"), "Backticks are not allowed in ... names")
      new NonBacktickName(s)
    }
  }

  implicit val propertyKeyEncoder = new KeyEncoder[NonBacktickName] {
    override def apply(pKey: NonBacktickName): String = pKey.name
  }
  implicit val propertyKeyDecoder = new KeyDecoder[NonBacktickName] {
    override def apply(str: String): Option[NonBacktickName] = Some(NonBacktickName(str))
  }

  type ParameterValue = Json
  type ParameterMap = Map[PropertyKey, ParameterValue]

  type PropertyValue = Json
  type PropertyMap = Map[PropertyKey, PropertyValue]
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]

  object ParameterMap {
    def apply(tuples: (String, ParameterValue)*): ParameterMap = Map(tuples.map { case (k,v) => PropertyKey(k) -> v }: _*)
    def empty = Map.empty[PropertyKey, ParameterValue]
  }
  val PropertyMap = ParameterMap
}
