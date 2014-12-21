package renesca.helpers

import renesca.{Graph, GraphChange, PropertyValue}

import scala.collection.mutable

class Properties(val id: Long,
                 setPropertyChange: (Long, String, PropertyValue) => GraphChange,
                 removePropertyChange: (Long, String) => GraphChange,
                 self: mutable.Map[String, PropertyValue] = mutable.HashMap.empty[String, PropertyValue])
  extends mutable.Map[String, PropertyValue] with mutable.MapLike[String, PropertyValue, Properties] {

  var graph: Graph = null

  override def +=(keyValue: (String, PropertyValue)) = {
    self += keyValue
    graph.changes += setPropertyChange(id, keyValue._1, keyValue._2)
    this
  }

  override def -=(key: String) = {
    self -= key
    graph.changes += removePropertyChange(id, key)
    this
  }

  override def get(key: String): Option[PropertyValue] = self.get(key)
  override def empty: Properties = new Properties(id, setPropertyChange, removePropertyChange, self.empty)
  override def iterator: Iterator[(String, PropertyValue)] = self.iterator
}
