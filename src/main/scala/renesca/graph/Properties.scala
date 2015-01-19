package renesca.graph

import renesca.json.{PropertyKey, PropertyValue}

import scala.collection.mutable

class Properties(val id: Long,
  setPropertyChange: (Long, PropertyKey, PropertyValue) => GraphChange,
  removePropertyChange: (Long, PropertyKey) => GraphChange,
  self: mutable.Map[PropertyKey, PropertyValue] = mutable.HashMap.empty[PropertyKey, PropertyValue])

  extends mutable.Map[PropertyKey, PropertyValue] with mutable.MapLike[PropertyKey, PropertyValue, Properties] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(keyValue: (PropertyKey, PropertyValue)) = {
    self += keyValue
    localChanges += setPropertyChange(id, keyValue._1, keyValue._2)
    this
  }

  override def -=(key: PropertyKey) = {
    self -= key
    localChanges += removePropertyChange(id, key)
    this
  }

  override def get(key: PropertyKey): Option[PropertyValue] = self.get(key)
  override def empty: Properties = new Properties(id, setPropertyChange, removePropertyChange, self.empty)
  override def iterator: Iterator[(PropertyKey, PropertyValue)] = self.iterator
}
