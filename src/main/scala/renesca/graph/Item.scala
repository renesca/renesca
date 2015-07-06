package renesca.graph

import renesca.parameter.PropertyKey

import scala.PartialFunction._

trait SubGraph {
  def origin: Origin
}

trait Item extends SubGraph {
  val properties: Properties
  var origin: Origin
}

object OriginKind extends Enumeration {
  type OriginKind = Value
  val MATCH, CREATE, MERGE, ID = Value
}

import renesca.graph.OriginKind._

sealed trait Origin {
  def isLocal: Boolean
  def kind: OriginKind
}

sealed trait LocalOrigin extends Origin {
  def isLocal = true
}

// origins are only equal if they have the same id
class Create() extends LocalOrigin {
  val kind = Create.kind
  override def toString = "Create()"
}

class Merge(val properties: Set[PropertyKey], val onMatch: Set[PropertyKey]) extends LocalOrigin {
  val kind = Merge.kind
  override def toString = s"Merge($properties, $onMatch)"
}

class Match(val properties: Set[PropertyKey]) extends LocalOrigin {
  val kind = Match.kind
  override def toString = s"Match($properties)"
}

case class Id(id: Long) extends Origin {
  val kind = Id.kind
  def isLocal = false
  override def toString = id.toString
}

object Create {
  val kind = OriginKind.CREATE
  def apply() = new Create
  def unapply(origin: Origin) = origin.isInstanceOf[Create]
}

object Merge {
  val kind = OriginKind.MERGE
  def apply(properties: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty) = new Merge(properties, onMatch)
  def unapply(origin: Origin) = condOpt(origin) {
    case m: Merge => (m.properties, m.onMatch)
  }
}

object Match {
  val kind = OriginKind.MATCH
  def apply(properties: Set[PropertyKey]) = new Match(properties)
  def unapply(origin: Origin) = condOpt(origin) {
    case m: Match => (m.properties)
  }
}

object Id {
  val kind = OriginKind.ID

  implicit def LongToId(id: Long): Id = Id(id)
  implicit def IntToId(id: Int): Id = Id(id)
}
