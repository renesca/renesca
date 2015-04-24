package renesca.schema

// HyperRelations behave like Nodes and Relations at the same time.
// This allows to create a Relation between a Node and a HyperRelation.

import renesca.{graph => raw}
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._

sealed trait Item

trait Node extends Item with NodeFilter {
  def label = node.labels.head
  def node: raw.Node
  implicit var graph: raw.Graph = null

  def neighboursAs[T <: Node](nodeFactory: NodeFactory[T]) = filterNodes(node.neighbours, nodeFactory)
  def successorsAs[T <: Node](nodeFactory: NodeFactory[T]) = filterNodes(node.successors, nodeFactory)
  def predecessorsAs[T <: Node](nodeFactory: NodeFactory[T]) = filterNodes(node.predecessors, nodeFactory)
  def getStringProperty(key: String) = node.properties(key).asInstanceOf[StringPropertyValue]
}

trait AbstractRelation[+STARTNODE <: Node, +ENDNODE <: Node] {
  def startNode: STARTNODE
  def endNode: ENDNODE
}

trait Relation[+STARTNODE <: Node, +ENDNODE <: Node] extends Item with AbstractRelation[STARTNODE, ENDNODE] {
  def relation: raw.Relation
  def relationType: raw.RelationType = relation.relationType
}


trait HyperRelation[
+STARTNODE <: Node,
STARTRELATION <: Relation[STARTNODE, HYPERRELATION],
HYPERRELATION <: HyperRelation[STARTNODE, STARTRELATION, HYPERRELATION, ENDRELATION, ENDNODE],
ENDRELATION <: Relation[HYPERRELATION, ENDNODE],
+ENDNODE <: Node]
  extends Item with AbstractRelation[STARTNODE, ENDNODE] with Node {
  // wraps a node and two relations
  protected[schema] var _startRelation: STARTRELATION = _
  protected[schema] var _endRelation: ENDRELATION = _

  def startRelation = _startRelation
  def endRelation = _endRelation
  def startNode = startRelation.startNode
  def endNode = endRelation.endNode
}
