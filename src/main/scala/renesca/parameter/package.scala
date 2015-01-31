package renesca

import scala.collection.mutable

package object parameter {
  type PropertyMap = Map[PropertyKey, PropertyValue]
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]
  type ParameterMap = Map[PropertyKey,ParameterValue]

  case class PropertyKey(name:String) extends NonBacktickName {
    override def equals(other: Any): Boolean = other match {
      case that: PropertyKey => this.name == that.name
      case that: String => name == that
      case _ => false
    }
    override def hashCode = name.hashCode
  }

  sealed trait ParameterValue
  sealed trait SoleParameterValue extends ParameterValue
  sealed trait PropertyValue extends ParameterValue

  final case class LongPropertyValue(value:Long) extends PropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: LongPropertyValue => this.value == that.value
      case that: Int => value == that
      case that: Long => value == that
      case _ => false
    }
    override def hashCode = value.hashCode
  }

  final case class DoublePropertyValue(value:Double) extends PropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: DoublePropertyValue => this.value == that.value
      case that: Double => value == that
      case _ => false
    }
    override def hashCode = value.hashCode
  }
  case class StringPropertyValue(value:String) extends PropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: StringPropertyValue => this.value == that.value
      case that: String => value == that
      case _ => false
    }
    override def hashCode = value.hashCode
  }
  case class BooleanPropertyValue(value:Boolean) extends PropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: BooleanPropertyValue => this.value == that.value
      case that: Boolean => value == that
      case _ => false
    }
    override def hashCode = value.hashCode
  }
  case class ArrayPropertyValue(value:Seq[PropertyValue]) extends PropertyValue {
    //TODO: forbid nesting of propertyvalues
    override def equals(other: Any): Boolean = other match {
      case that: ArrayPropertyValue => this.value == that.value
      case that: Seq[_] => value.sameElements(that)
      case _ => false
    }
    override def hashCode = value.hashCode
  }

  case class ArrayParameterValue(value:Seq[ParameterValue]) extends SoleParameterValue {
    override def equals(other: Any): Boolean = other match {
      case that: ArrayParameterValue => this.value == that.value
      case that: Seq[_] => value.sameElements(that)
      case _ => false
    }
    override def hashCode = value.hashCode
  }
  case class MapParameterValue(value:ParameterMap) extends SoleParameterValue {
    override def equals(other: Any): Boolean = other match {
      case that: MapParameterValue => this.value == that.value
      case that: Map[_,_] => value.sameElements(that)
      case _ => false
    }
    override def hashCode = value.hashCode
  }

}
