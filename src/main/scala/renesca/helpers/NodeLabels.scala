package renesca.helpers

import renesca.{Graph, Label, NodeRemoveLabel, NodeSetLabel}

import scala.collection.mutable

abstract class NodeLabels extends mutable.HashSet[Label] {
   var graph: Graph = null
   val id: Long

   override def +=(elem:Label) = {
     graph.changes += NodeSetLabel(id, elem)
     super.+=(elem)
   }

   override def -=(elem:Label) = {
     graph.changes += NodeRemoveLabel(id, elem)
     super.-=(elem)
   }
 }
