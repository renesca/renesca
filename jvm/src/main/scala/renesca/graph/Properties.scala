package renesca.graph


import renesca.parameter.{MutablePropertyMap, PropertyKey, PropertyValue}

import scala.collection.mutable

class Properties(val item: Item,
                 self: MutablePropertyMap = mutable.HashMap.empty)
  extends MutablePropertyMap with mutable.MapLike[PropertyKey, PropertyValue, Properties] {

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  override def +=(keyValue: (PropertyKey, PropertyValue)) = {
    self += keyValue
    if(!item.origin.isLocal)
      localChanges += SetProperty(item, keyValue._1, keyValue._2)
    this
  }

  override def -=(key: PropertyKey) = {
    self -= key
    if(!item.origin.isLocal)
      localChanges += RemoveProperty(item, key)
    this
  }

  override def get(key: PropertyKey): Option[PropertyValue] = self.get(key)
  override def empty: Properties = new Properties(item, self.empty)
  override def iterator: Iterator[(PropertyKey, PropertyValue)] = self.iterator
}
