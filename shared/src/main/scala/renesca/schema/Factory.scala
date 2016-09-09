package renesca.schema

// Traits for Factories to create either local items or instances of existing ones.

import renesca.{graph => raw}

trait NodeFactory[+T <: Node] {
  def label: raw.Label
  def labels: Set[raw.Label]
  def wrap(node: raw.Node): T
}

trait AbstractRelationFactory[+START <: Node, +RELATION <: AbstractRelation[START, END] with Item, +END <: Node]

trait RelationFactory[+START <: Node, +RELATION <: Relation[START, END], +END <: Node] extends AbstractRelationFactory[START, RELATION, END] {
  def relationType: raw.RelationType
  def wrap(relation: raw.Relation): RELATION
}

trait HyperRelationFactory[
+START <: Node,
STARTRELATION <: Relation[START, HYPERRELATION],
HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
ENDRELATION <: Relation[HYPERRELATION, END],
+END <: Node] extends NodeFactory[HYPERRELATION] with AbstractRelationFactory[START, HYPERRELATION, END] {

  def startRelationType: raw.RelationType
  def endRelationType: raw.RelationType

  def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation): HYPERRELATION
}

