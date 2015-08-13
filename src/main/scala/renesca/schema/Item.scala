package renesca.schema

// HyperRelations behave like Nodes and Relations at the same time.
// This allows to create a Relation between a Node and a HyperRelation.

import renesca.{graph => raw}

//TODO: sealed trait Item and AbstractRelation?
trait Item {
  def rawItem: raw.Item

  override def equals(other: Any): Boolean = other match {
    case that: Item =>
      rawItem.equals(that.rawItem)
    case _          => false
  }

  override def hashCode(): Int = 31 * rawItem.hashCode()

  def validate: Option[String] = None
}

trait Node extends Item with Filter {
  val label: raw.Label
  val labels: Set[raw.Label]
  def rawItem: raw.Node
  private[schema] var graphOption: Option[raw.Graph] = None
  private val emptyGraph = raw.Graph.empty

  implicit def graph: raw.Graph = graphOption.getOrElse(emptyGraph)

  def neighboursAs[T <: Node](nodeFactory: NodeFactory[T]) = filterNodes(rawItem.neighbours, nodeFactory)
  def successorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], relationFactory: RelationFactory[_, _, RELNODE]) = {
    filterNodes(rawItem.outRelations.filter(_.relationType == relationFactory.relationType).map(_.endNode), nodeFactory)
  }
  def predecessorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], relationFactory: RelationFactory[RELNODE, _, _]) = {
    filterNodes(rawItem.inRelations.filter(_.relationType == relationFactory.relationType).map(_.startNode), nodeFactory)
  }
  def successorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], hyperRelationFactory: HyperRelationFactory[_, _, _, _, RELNODE]) = {
    filterNodes(rawItem.outRelations.filter(_.relationType == hyperRelationFactory.startRelationType).map(_.endNode).filter(_.labels contains hyperRelationFactory.label).flatMap(_.outRelations.filter(_.relationType == hyperRelationFactory.endRelationType).map(_.endNode)), nodeFactory)
  }
  def predecessorsAs[RELNODE <: Node, NODE <: RELNODE](nodeFactory: NodeFactory[NODE], hyperRelationFactory: HyperRelationFactory[RELNODE, _, _, _, _]) = {
    filterNodes(rawItem.inRelations.filter(_.relationType == hyperRelationFactory.endRelationType).map(_.startNode).filter(_.labels contains hyperRelationFactory.label).flatMap(_.inRelations.filter(_.relationType == hyperRelationFactory.startRelationType).map(_.startNode)), nodeFactory)
  }

  def inRelationsAs[START <: Node, RELATION <: Relation[START, END], END <: Node]
  (relationFactory: RelationFactory[START, RELATION, END]): Seq[RELATION] = {
    filterRelations(rawItem.inRelations, relationFactory)
  }

  def outRelationsAs[START <: Node, RELATION <: Relation[START, END], END <: Node]
  (relationFactory: RelationFactory[START, RELATION, END]): Seq[RELATION] = {
    filterRelations(rawItem.outRelations, relationFactory)
  }

  def relationsAs[START <: Node, RELATION <: Relation[START, END], END <: Node]
  (relationFactory: RelationFactory[START, RELATION, END]): Seq[RELATION] = {
    filterRelations(rawItem.relations, relationFactory)
  }

  def inRelationsAs[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END]): Seq[HYPERRELATION] = {
    filterHyperRelations(rawItem.predecessors, graph.relations, hyperRelationFactory)
  }

  def outRelationsAs[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END]): Seq[HYPERRELATION] = {
    filterHyperRelations(rawItem.successors, graph.relations, hyperRelationFactory)
  }

  def relationsAs[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END]): Seq[HYPERRELATION] = {
    filterHyperRelations(rawItem.neighbours, graph.relations, hyperRelationFactory)
  }
}

trait AbstractRelation[+START <: Node, +END <: Node] extends Item {
  def startNodeOpt: Option[START]
  def endNodeOpt: Option[END]
}

trait Relation[+START <: Node, +END <: Node] extends AbstractRelation[START, END] {
  def startNode: START
  def endNode: END
  def startNodeOpt = Some(startNode)
  def endNodeOpt = Some(endNode)
  def rawItem: raw.Relation
  def relationType: raw.RelationType = rawItem.relationType
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

  def rawPath: Option[raw.Path] = {
    if(startRelationOpt.isDefined && endRelationOpt.isDefined)
      raw.Path(startRelationOpt.get.rawItem, endRelationOpt.get.rawItem).right.toOption
    else
      None
  }

  def startRelationOpt = Option(_startRelation)
  def endRelationOpt = Option(_endRelation)
  def startNodeOpt = startRelationOpt.map(_.startNode)
  def endNodeOpt = endRelationOpt.map(_.endNode)
}

