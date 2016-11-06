package renesca.schema

// Traits for Factories to create either local items or instances of existing ones.

import renesca.{graph => raw}
import renesca.{Label,RelationType}

trait NodeFactory[+T <: Node] {
  def label: Label
  def labels: Set[Label]
  def wrap(node: raw.Node): T
}

trait AbstractRelationFactory[+START <: Node, +RELATION <: AbstractRelation[START, END] with Item, +END <: Node]

trait RelationFactory[+START <: Node, +RELATION <: Relation[START, END], +END <: Node] extends AbstractRelationFactory[START, RELATION, END] {
  def relationType: RelationType
  def wrap(relation: raw.Relation): RELATION
}

trait HyperRelationFactory[
+START <: Node,
STARTRELATION <: Relation[START, HYPERRELATION],
HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
ENDRELATION <: Relation[HYPERRELATION, END],
+END <: Node] extends NodeFactory[HYPERRELATION] with AbstractRelationFactory[START, HYPERRELATION, END] {

  def startRelationType: RelationType
  def endRelationType: RelationType

  def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation): HYPERRELATION
}

