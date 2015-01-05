package renesca.graph

import renesca.json.Value

import scala.collection.mutable

class Properties(val id: Long,
  setPropertyChange: (Long, String, Value) => GraphChange,
  removePropertyChange: (Long, String) => GraphChange,
  self: mutable.Map[String, Value] = mutable.HashMap.empty[String, Value])

  extends mutable.Map[String, Value] with mutable.MapLike[String, Value, Properties] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(keyValue: (String, Value)) = {
    self += keyValue
    localChanges += setPropertyChange(id, keyValue._1, keyValue._2)
    this
  }

  override def -=(key: String) = {
    self -= key
    localChanges += removePropertyChange(id, key)
    this
  }

  override def get(key: String): Option[Value] = self.get(key)
  override def empty: Properties = new Properties(id, setPropertyChange, removePropertyChange, self.empty)
  override def iterator: Iterator[(String, Value)] = self.iterator
}
