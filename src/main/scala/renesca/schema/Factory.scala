package renesca.schema

// Traits for Factories to create either local items or instances of existing ones.

import renesca.{graph => raw}
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._

trait NodeFactory[+T <: Node] {
  def label: raw.Label
  def create(node: raw.Node): T
  def local: T = create(raw.Node.local(List(label)))
}

trait AbstractRelationFactory[STARTNODE <: Node, RELATION <: AbstractRelation[STARTNODE, ENDNODE] with Item, ENDNODE <: Node] {
  def local(startNode: STARTNODE, endNode: ENDNODE): RELATION
  def startNodeFactory: NodeFactory[STARTNODE]
  def endNodeFactory: NodeFactory[ENDNODE]
}

trait RelationFactory[STARTNODE <: Node, RELATION <: Relation[STARTNODE, ENDNODE], ENDNODE <: Node] extends AbstractRelationFactory[STARTNODE, RELATION, ENDNODE] {
  def relationType: raw.RelationType
  def create(relation: raw.Relation): RELATION
  def local(startNode: STARTNODE, endNode: ENDNODE): RELATION = {
    create(raw.Relation.local(startNode.node, relationType, endNode.node))
  }
}

trait HyperRelationFactory[
STARTNODE <: Node,
STARTRELATION <: Relation[STARTNODE, HYPERRELATION],
HYPERRELATION <: HyperRelation[STARTNODE, STARTRELATION, HYPERRELATION, ENDRELATION, ENDNODE],
ENDRELATION <: Relation[HYPERRELATION, ENDNODE],
ENDNODE <: Node] extends NodeFactory[HYPERRELATION] with AbstractRelationFactory[STARTNODE, HYPERRELATION, ENDNODE] {

  def startRelationType: raw.RelationType
  def endRelationType: raw.RelationType

  def factory: NodeFactory[HYPERRELATION]

  def startRelationCreate(relation: raw.Relation): STARTRELATION
  def endRelationCreate(relation: raw.Relation): ENDRELATION
  def create(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation): HYPERRELATION = {
    val hyperRelation = create(middleNode)
    hyperRelation._startRelation = startRelationCreate(startRelation)
    hyperRelation._endRelation = endRelationCreate(endRelation)
    hyperRelation
  }

  def startRelationLocal(startNode: STARTNODE, middleNode: HYPERRELATION): STARTRELATION = {
    startRelationCreate(raw.Relation.local(startNode.node, startRelationType, middleNode.node))
  }

  def endRelationLocal(middleNode: HYPERRELATION, endNode: ENDNODE): ENDRELATION = {
    endRelationCreate(raw.Relation.local(middleNode.node, endRelationType, endNode.node))
  }

  def local(startNode: STARTNODE, endNode: ENDNODE): HYPERRELATION = {
    val middleNode = super[NodeFactory].local
    create(startRelationLocal(startNode, middleNode).relation, middleNode.node, endRelationLocal(middleNode, endNode).relation)
  }
}
