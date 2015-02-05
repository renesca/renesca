package renesca.graph

import renesca.json
import renesca.parameter.{PropertyMap, PropertyValue}

import scala.collection.mutable

object Id {
  import renesca.parameter.implicits._

  implicit def IdToPropertyValue(id: Id):PropertyValue = id.value
  implicit def IdToLong(id: Id):Long = id.value
  implicit def LongToId(id: Long):Id = Id(id)
  implicit def IntToId(id: Int):Id = Id(id)

  private var currentLocalId:Long = -1

  private[graph] def nextId() = {
    val newId = currentLocalId
    currentLocalId -= 1
    Id(newId)
  }
}

case class Id(private var _id: Long) {
  def value = _id
  def value_=(id: Long) {
    _id = id
  }

  override def equals(other: Any): Boolean = other match {
    case that: Id => _id == that._id
    case that: Long => _id == that
    case that: Int => _id == that
    case _ => false
  }
  override def hashCode = _id.hashCode

  override def toString = _id.toString
}

object Graph {
  def apply(nodes: Traversable[Node], relations: Traversable[Relation] = Nil):Graph = {
    new Graph(
      mutable.LinkedHashSet.empty ++ nodes,
      mutable.LinkedHashSet.empty ++ relations)
  }

  def apply(jsonGraph:json.Graph):Graph = {
    val nodes:List[Node] = jsonGraph.nodes.map{ case json.Node(id,labels,properties) =>
      Node(id.toLong, labels.map(Label.apply), properties)
    }

    val idToNode:Map[String,Node] = nodes.map{case node => node.id.toString -> node}.toMap

    val relations:List[Relation] = jsonGraph.relationships.map {
      case json.Relationship(id, relationshipType, startNode, endNode, properties) =>
      Relation(id.toLong,
        idToNode(startNode),
        idToNode(endNode),
        RelationType(relationshipType),
        properties)
    }

    Graph(nodes, relations)
  }

  def empty = new Graph(mutable.LinkedHashSet.empty, mutable.LinkedHashSet.empty)
}


class Graph private[graph] (val nodes: mutable.LinkedHashSet[Node], val relations: mutable.LinkedHashSet[Relation]) {
  // private constructor to force usage of Factory

  // graph must be consistent
  require(relations.forall{ relation =>
    (nodes contains relation.startNode) &&
    (nodes contains relation.endNode)
  }) // TODO: test

  private[graph] val localChanges = mutable.ArrayBuffer.empty[GraphChange]

  def changes: Seq[GraphChange] = {
    val unsortedChanges = localChanges ++
    (nodes.flatMap(node => node.changes) ++
      relations.flatMap(relation => relation.changes))
    unsortedChanges.sortBy(_.timestamp)
  }

  def clearChanges() {
    localChanges.clear()
    nodes.foreach{node =>
      node.properties.localChanges.clear()
      node.labels.localChanges.clear()
    }
    relations.foreach{relation =>
      relation.properties.localChanges.clear()
    }
  }

  def addRelation(start: Node, end: Node, relationType: RelationType, properties: PropertyMap = Map.empty) = {
    val relation = Relation.local(start, end, relationType)
    relations += relation
    localChanges += RelationAdd(relation.id, start.id, end.id, relationType)
    relation.properties ++= properties
    relation
  }

  def addNode(labels: Traversable[Label] = Nil, properties: PropertyMap = Map.empty):Node = {
    val node = Node.local
    nodes += node
    localChanges += NodeAdd(node.id)
    node.properties ++= properties
    node.labels ++= labels
    node
  }

  def delete(relation: Relation) {
    relations -= relation
    localChanges += RelationDelete(relation.id)
  }

  def delete(node: Node) {
    nodes -= node
    relations --= incidentRelations(node)
    localChanges += NodeDelete(node.id)
  }

  def outRelations(node: Node) = relations.filter(node == _.startNode).toSet
  def inRelations(node: Node) = relations.filter(node == _.endNode).toSet
  def incidentRelations(node: Node): Set[Relation] = inRelations(node) ++ outRelations(node)
  def neighbours(node: Node) = incidentRelations(node).map(_.other(node))
  def successors(node: Node) = outRelations(node).map(_.endNode)
  def predecessors(node: Node) = inRelations(node).map(_.startNode)
  def inDegree(node: Node) = inRelations(node).size
  def outDegree(node: Node) = outRelations(node).size
  def degree(node: Node) = inDegree(node) + outDegree(node)

  def merge(that:Graph) = {
    val graph = Graph(this.nodes ++ that.nodes, this.relations ++ that.relations)
    graph.localChanges ++= this.changes
    graph.localChanges ++= that.changes
    graph
  }

  def isEmpty = nodes.isEmpty
  def nonEmpty = nodes.nonEmpty

  override def toString = s"Graph(nodes:(${nodes.map(_.id).mkString(", ")}), relations:(${relations.map( r => s"${r.id}:${r.startNode.id}->${r.endNode.id}").mkString(", ")}))"

  def canEqual(other: Any): Boolean = other.isInstanceOf[Graph]

  override def equals(other: Any): Boolean = other match {
    case that: Graph =>
      (that canEqual this) &&
        this.nodes == that.nodes &&
        this.relations == that.relations
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(nodes, relations)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}






