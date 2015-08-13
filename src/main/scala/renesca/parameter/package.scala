package renesca

// These classes define data structures for Properties and Parameters.
// They aim to be transparent with their contents in usage and equality.
//
// Properties are either primitives[Long,Double,String,Boolean] or Arrays of these.
//   Elements in Property-Arrays must have the same primitive type.
//   They are used for properties in Neo4j Nodes and Relations.
// Parameters are either primitives[Long,Double,String,Boolean], Arrays of Parameters or Maps of Parameters.
//   They can be arbitrarily nested. Arrays can have elements of different types.
//   As the primitives are the same as in Properties, this implementation is reused for Parameters.
//   Paramaters are used for Cypher query parameters and data in rows of query results.

import scala.collection.mutable

package object parameter {
  type PropertyMap = Map[PropertyKey, PropertyValue]
  type MutablePropertyMap = mutable.Map[PropertyKey, PropertyValue]
  type ParameterMap = Map[PropertyKey, ParameterValue]

  case class PropertyKey(name: String) extends NonBacktickName {
    override def equals(other: Any): Boolean = other match {
      case that: PropertyKey => this.name == that.name
      case that: String      => name == that
      case _                 => false
    }
    override def hashCode = name.hashCode
  }

  sealed trait ParameterValue {
    def asString: String = this.asInstanceOf[StringPropertyValue].value
    def asLong: Long = this.asInstanceOf[LongPropertyValue].value
    def asDouble: Double = this.asInstanceOf[DoublePropertyValue].value
    def asBoolean: Boolean = this.asInstanceOf[BooleanPropertyValue].value

    def asStringArray: Seq[String] = this.asInstanceOf[StringArrayPropertyValue].elements.map(_.value)
    def asLongArray: Seq[Long] = this.asInstanceOf[LongArrayPropertyValue].elements.map(_.value)
    def asDoubleArray: Seq[Double] = this.asInstanceOf[DoubleArrayPropertyValue].elements.map(_.value)
    def asBooleanArray: Seq[Boolean] = this.asInstanceOf[BooleanArrayPropertyValue].elements.map(_.value)

    def asMap: Map[String, ParameterValue] = this.asInstanceOf[MapParameterValue].value.map { case (k, v) => k.name -> v }
  }

  sealed trait SoleParameterValue extends ParameterValue

  sealed trait PropertyValue extends ParameterValue

  sealed trait PrimitivePropertyValue extends PropertyValue

  sealed trait ArrayPropertyValue extends PropertyValue {
    def elements: Seq[PrimitivePropertyValue]
    override def toString = elements.map(_.toString).toString
  }

  case class LongPropertyValue(value: Long) extends PrimitivePropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: LongPropertyValue => this.value == that.value
      case that: Int               => value == that
      case that: Long              => value == that
      case _                       => false
    }
    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  case class DoublePropertyValue(value: Double) extends PrimitivePropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: DoublePropertyValue => this.value == that.value
      case that: Double              => value == that
      case _                         => false
    }
    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  case class StringPropertyValue(value: String) extends PrimitivePropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: StringPropertyValue => this.value == that.value
      case that: String              => value == that
      case _                         => false
    }
    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  case class BooleanPropertyValue(value: Boolean) extends PrimitivePropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: BooleanPropertyValue => this.value == that.value
      case that: Boolean              => value == that
      case _                          => false
    }
    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  case object NullPropertyValue extends PrimitivePropertyValue {
    //TODO: equals/hashCode/implicits?
    override def toString = "null"
  }

  case class LongArrayPropertyValue(elements: LongPropertyValue*) extends ArrayPropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: LongArrayPropertyValue => this.elements == that.elements
      case that: ArrayParameterValue    => this.elements == that.value
      case that: Seq[_]                 => elements == that
      case _                            => false
    }
    override def hashCode = elements.hashCode
  }

  case class DoubleArrayPropertyValue(elements: DoublePropertyValue*) extends ArrayPropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: DoubleArrayPropertyValue => this.elements == that.elements
      case that: ArrayParameterValue      => this.elements == that.value
      case that: Seq[_]                   => elements == that
      case _                              => false
    }
    override def hashCode = elements.hashCode
  }

  case class StringArrayPropertyValue(elements: StringPropertyValue*) extends ArrayPropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: StringArrayPropertyValue => this.elements == that.elements
      case that: ArrayParameterValue      => this.elements == that.value
      case that: Seq[_]                   => elements == that
      case _                              => false
    }
    override def hashCode = elements.hashCode
  }

  case class BooleanArrayPropertyValue(elements: BooleanPropertyValue*) extends ArrayPropertyValue {
    override def equals(other: Any): Boolean = other match {
      case that: BooleanArrayPropertyValue => this.elements == that.elements
      case that: ArrayParameterValue       => this.elements == that.value
      case that: Seq[_]                    => elements == that
      case _                               => false
    }
    override def hashCode = elements.hashCode
  }

  case class ArrayParameterValue(value: Seq[ParameterValue]) extends SoleParameterValue {
    override def equals(other: Any): Boolean = other match {
      case that: ArrayParameterValue       => this.value == that.value
      case that: LongArrayPropertyValue    => this.value == that.elements
      case that: DoubleArrayPropertyValue  => this.value == that.elements
      case that: StringArrayPropertyValue  => this.value == that.elements
      case that: BooleanArrayPropertyValue => this.value == that.elements
      case that: Seq[_]                    => value == that
      case _                               => false
    }
    override def hashCode = value.hashCode
    override def toString = value.map(_.toString).toString
  }

  case class MapParameterValue(value: ParameterMap) extends SoleParameterValue {
    override def equals(other: Any): Boolean = other match {
      case that: MapParameterValue => this.value == that.value
      case that: Map[_, _]         => value == that
      case _                       => false
    }
    override def hashCode = value.hashCode
    override def toString = value.toString
  }

}
