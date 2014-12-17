package renesca.helpers

import renesca.{Graph, GraphChange, PropertyValue}

import scala.collection.mutable

abstract class Properties extends mutable.HashMap[String, PropertyValue] {
   var graph: Graph = null
   val id: Long
   val setPropertyChange:(Long,String,PropertyValue) => GraphChange
   val removePropertyChange:(Long,String) => GraphChange

   override def +=(keyValue:(String, PropertyValue)) = {
     graph.changes += setPropertyChange(id, keyValue._1, keyValue._2)
     super.+=(keyValue)
   }

   override def -=(key:String) = {
     graph.changes += removePropertyChange(id, key)
     super.-=(key)
   }
 }
