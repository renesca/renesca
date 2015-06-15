package renesca.graph

import renesca.parameter.PropertyKey

trait Item {
  val id: Id
  val properties: Properties
}

trait ItemOrigin

case class Create() extends ItemOrigin
case class Merge(properties: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey]) extends ItemOrigin
case class Match() extends ItemOrigin
