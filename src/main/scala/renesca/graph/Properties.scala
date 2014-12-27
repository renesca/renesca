package renesca.graph

import scala.collection.mutable

class Properties(val id: Long,
  setPropertyChange: (Long, String, PropertyValue) => GraphChange,
  removePropertyChange: (Long, String) => GraphChange,
  self: mutable.Map[String, PropertyValue] = mutable.HashMap.empty[String, PropertyValue])

  extends mutable.Map[String, PropertyValue] with mutable.MapLike[String, PropertyValue, Properties] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(keyValue: (String, PropertyValue)) = {
    self += keyValue
    localChanges += setPropertyChange(id, keyValue._1, keyValue._2)
    this
  }

  override def -=(key: String) = {
    self -= key
    localChanges += removePropertyChange(id, key)
    this
  }

  override def get(key: String): Option[PropertyValue] = self.get(key)
  override def empty: Properties = new Properties(id, setPropertyChange, removePropertyChange, self.empty)
  override def iterator: Iterator[(String, PropertyValue)] = self.iterator
}
