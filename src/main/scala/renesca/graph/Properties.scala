package renesca.graph


import renesca.parameter.{MutablePropertyMap, PropertyKey, PropertyValue}

import scala.collection.mutable

class Properties(val id: Id,
                 setPropertyChange: (Id, PropertyKey, PropertyValue) => GraphChange,
                 removePropertyChange: (Id, PropertyKey) => GraphChange,
                 self: MutablePropertyMap = mutable.HashMap.empty)
  extends MutablePropertyMap with mutable.MapLike[PropertyKey, PropertyValue, Properties] {

  private var _unique: Option[Seq[PropertyKey]] = None
  def unique = _unique
  def unique_=(values: Seq[PropertyKey]) = _unique = Some(values)

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(keyValue: (PropertyKey, PropertyValue)) = {
    self += keyValue
    if (!id.isLocal)
      localChanges += setPropertyChange(id, keyValue._1, keyValue._2)
    this
  }

  override def -=(key: PropertyKey) = {
    self -= key
    if (!id.isLocal)
      localChanges += removePropertyChange(id, key)
    this
  }

  override def get(key: PropertyKey): Option[PropertyValue] = self.get(key)
  override def empty: Properties = new Properties(id, setPropertyChange, removePropertyChange, self.empty)
  override def iterator: Iterator[(PropertyKey, PropertyValue)] = self.iterator
}
