package object renesca {

  import collection.mutable
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  // TODO: type TransactionId = String
  type ParameterValue = Json
  type ParameterMap = Map[String, ParameterValue]

  //TODO: value class?
  case class PropertyKey(name: String) extends NonBacktickName {
    override def equals(other: Any): Boolean = other match {
      case that: PropertyKey => this.name == that.name
      case that: String => name == that
      case _ => false
    }
    override def hashCode = name.hashCode
  }

  implicit val propertyKeyEncoder = new KeyEncoder[PropertyKey] {
    override def apply(pKey: PropertyKey): String = pKey.name
  }
  implicit val propertyKeyDecoder = new KeyDecoder[PropertyKey] {
    override def apply(str: String): Option[PropertyKey] = Some(PropertyKey(str))
  }

  type PropertyValue = Json
  type PropertyMap = Map[PropertyKey, PropertyValue]
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]
}
