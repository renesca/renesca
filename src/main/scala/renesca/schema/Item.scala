package renesca.schema

// HyperRelations behave like Nodes and Relations at the same time.
// This allows to create a Relation between a Node and a HyperRelation.

import renesca.{graph => raw}
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._

//TODO: sealed trait Item and AbstractRelation?
trait Item

trait Node extends Item with Filter {
  def label = node.labels.head
  def node: raw.Node
  implicit var graph: raw.Graph = null

  def neighboursAs[T <: Node](nodeFactory: NodeFactory[T]) = filterNodes(node.neighbours, nodeFactory)
  def successorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], relationFactory: RelationFactory[_, _, RELNODE]) = {
    filterNodes(node.outRelations.filter(_.relationType == relationFactory.relationType).map(_.endNode), nodeFactory)
  }
  //TODO: successorsAs for HyperRelations, same for predecessors
  def predecessorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], relationFactory: RelationFactory[RELNODE, _, _]) = {
    filterNodes(node.inRelations.filter(_.relationType == relationFactory.relationType).map(_.startNode), nodeFactory)
  }
  def getStringProperty(key: String) = node.properties(key).asInstanceOf[StringPropertyValue]
}

trait AbstractRelation[+START <: Node, +END <: Node] extends Item {
  def startNode: START
  def endNode: END
}

trait Relation[+START <: Node, +END <: Node] extends AbstractRelation[START, END] {
  def relation: raw.Relation
  def relationType: raw.RelationType = relation.relationType
}


trait HyperRelation[
+START <: Node,
STARTRELATION <: Relation[START, HYPERRELATION],
HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
ENDRELATION <: Relation[HYPERRELATION, END],
+END <: Node]
  extends AbstractRelation[START, END] with Node {
  // wraps a node and two relations
  protected[schema] var _startRelation: STARTRELATION = _
  protected[schema] var _endRelation: ENDRELATION = _

  def startRelation = _startRelation
  def endRelation = _endRelation
  def startNode = startRelation.startNode
  def endNode = endRelation.endNode
}

