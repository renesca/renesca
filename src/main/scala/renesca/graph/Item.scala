package renesca.graph

import renesca.parameter.PropertyKey

trait SubGraph {
  val origin: ItemOrigin
}

trait Item extends SubGraph {
  val id: Id
  val properties: Properties
}

trait ItemOrigin

case class Create() extends ItemOrigin
case class Merge(properties: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty) extends ItemOrigin
case class Match() extends ItemOrigin
