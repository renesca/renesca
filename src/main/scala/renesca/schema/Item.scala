package renesca.schema

// HyperRelations behave like Nodes and Relations at the same time.
// This allows to create a Relation between a Node and a HyperRelation.

import renesca.{graph => raw}

import scala.concurrent.Promise
import scala.util.Success

//TODO: sealed trait Item and AbstractRelation?
trait Item {
  def item: raw.Item
}

trait Node extends Item with Filter {
  val label: raw.Label
  val labels: Set[raw.Label]
  def node: raw.Node
  def item = node
  private[schema] var graphOption: Option[raw.Graph] = None
  private val emptyGraph = raw.Graph.empty

  implicit def graph: raw.Graph = graphOption.getOrElse(emptyGraph)

  def neighboursAs[T <: Node](nodeFactory: NodeFactory[T]) = filterNodes(node.neighbours, nodeFactory)
  def successorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], relationFactory: RelationFactory[_, _, RELNODE]) = {
    filterNodes(node.outRelations.filter(_.relationType == relationFactory.relationType).map(_.endNode), nodeFactory)
  }
  def predecessorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], relationFactory: RelationFactory[RELNODE, _, _]) = {
    filterNodes(node.inRelations.filter(_.relationType == relationFactory.relationType).map(_.startNode), nodeFactory)
  }
  def successorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], hyperRelationFactory: HyperRelationFactory[_, _, _, _, RELNODE]) = {
    filterNodes(node.outRelations.filter(_.relationType == hyperRelationFactory.startRelationType).map(_.endNode).filter(_.labels contains hyperRelationFactory.label).flatMap(_.outRelations.filter(_.relationType == hyperRelationFactory.endRelationType).map(_.endNode)), nodeFactory)
  }
  def predecessorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], hyperRelationFactory: HyperRelationFactory[RELNODE, _, _, _, _]) = {
    filterNodes(node.inRelations.filter(_.relationType == hyperRelationFactory.endRelationType).map(_.startNode).filter(_.labels contains hyperRelationFactory.label).flatMap(_.inRelations.filter(_.relationType == hyperRelationFactory.startRelationType).map(_.startNode)), nodeFactory)
  }
}

trait AbstractRelation[+START <: Node, +END <: Node] extends Item {
  def startNode: START
  def endNode: END
}

trait Relation[+START <: Node, +END <: Node] extends AbstractRelation[START, END] {
  def relation: raw.Relation
  def item = relation
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

  def path: Option[raw.Path] = {
    if(startRelation != null && endRelation != null)
      raw.Path(startRelation.relation, endRelation.relation).right.toOption
    else
      None
  }

  def startRelation = _startRelation
  def endRelation = _endRelation
  def startNode = startRelation.startNode
  def endNode = endRelation.endNode
}

