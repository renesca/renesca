package renesca

import scala.collection.mutable
import scala.collection.breakOut

package object parameter {
  type ParameterValue = AnyRef
  type PropertyValue = ParameterValue
  type ParameterMap = Map[PropertyKey, ParameterValue]
  type PropertyMap = ParameterMap
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]

  case class PropertyKey(name: String) extends NonBacktickName

  object ParameterMap {
    @inline private final def box(x: Any): AnyRef = x.asInstanceOf[AnyRef]
    def apply(tuples: (String, Any)*): ParameterMap = {
      tuples.map {
        case (k, v) => PropertyKey(k) -> box(v)
      }(breakOut[Seq[(String, Any)], (PropertyKey, AnyRef), Map[PropertyKey, AnyRef]])
    }

    def empty = Map.empty[PropertyKey, ParameterValue]
  }

  implicit def stringToPropertyKey(s: String): PropertyKey = PropertyKey(s)
}
