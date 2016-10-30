package renesca

import scala.collection.mutable

package object parameter {
  type ParameterValue = AnyRef
  type PropertyValue = ParameterValue
  type ParameterMap = Map[PropertyKey, ParameterValue]
  type PropertyMap = ParameterMap
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]

  case class PropertyKey(name: String) extends NonBacktickName

  object ParameterMap {
    def apply(tuples: (String, Any)*): ParameterMap = {
      Map(tuples.map { case (k,v) => PropertyKey(k) -> v.asInstanceOf[AnyRef] }: _*) //TODO: unboxed to boxed?
    }

    def empty = Map.empty[PropertyKey, ParameterValue]
  }

  implicit def stringToPropertyKey(s: String): PropertyKey = PropertyKey(s)
}
