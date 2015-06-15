package renesca.graph

import renesca.NonBacktickName
import renesca.parameter.{PropertyKey, PropertyMap, PropertyValue}

import scala.collection.mutable

object Label {
  implicit def StringToLabel(name: String): Label = Label(name)
}

case class Label(name: String) extends NonBacktickName

object Node {
  private[renesca] def apply(id: Id, labels: Traversable[Label] = Nil, properties: PropertyMap = Map.empty): Node = {
    new Node(id, Create(), labels, properties)
  }

  def create: Node = create()
  def create(labels: Traversable[Label] = Nil, properties: PropertyMap = Map.empty): Node = {
    apply(Id.nextId(), labels, properties)
  }

  def merge: Node = merge()
  def merge(labels: Traversable[Label] = Nil, properties: PropertyMap = Map.empty, merge: Set[PropertyKey] = Set.empty, onMatch: Set[PropertyKey] = Set.empty): Node = {
    new Node(Id.nextId(), Merge(merge, onMatch), labels, properties)
  }

  def find: Node = find()
  def find(labels: Traversable[Label] = Nil, properties: PropertyMap = Map.empty): Node = {
    new Node(Id.nextId(), Match(), labels, properties)
  }
}

class Node private[graph](val id: Id,
                          val origin: ItemOrigin,
                          initialLabels: Traversable[Label] = Nil,
                          initialProperties: PropertyMap = Map.empty
                           ) extends Item {

  val labels = new NodeLabels(this, mutable.HashSet(initialLabels.toSeq: _*))
  val properties = new Properties(this, mutable.Map(initialProperties.toSeq: _*))

  def changes: Seq[GraphChange] = labels.localChanges ++ properties.localChanges

  def outRelations(implicit graph: Graph) = graph.outRelations(this)
  def inRelations(implicit graph: Graph) = graph.inRelations(this)
  def relations(implicit graph: Graph) = graph.incidentRelations(this)
  def neighbours(implicit graph: Graph) = graph.neighbours(this)
  def successors(implicit graph: Graph) = graph.successors(this)
  def predecessors(implicit graph: Graph) = graph.predecessors(this)
  def inDegree(implicit graph: Graph) = graph.inDegree(this)
  def outDegree(implicit graph: Graph) = graph.outDegree(this)
  def degree(implicit graph: Graph) = graph.degree(this)

  def canEqual(other: Any): Boolean = other.isInstanceOf[Node]

  override def equals(other: Any): Boolean = other match {
    case that: Node =>
      (that canEqual this) &&
        this.id == that.id
    case _          => false
  }

  override def hashCode: Int = id.hashCode

  override def toString = s"(${ id }${ labels.map(":" + _.name).mkString })"
}

