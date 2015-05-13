package renesca.schema

import renesca.{graph => raw}
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._

trait NodeFilter {
  def graph: raw.Graph

  def filterNodes[T <: Node](nodes: Set[raw.Node], nodeFactory: NodeFactory[T]): Set[T] = {
    nodes.filter(_.labels.contains(nodeFactory.label)).map { node =>
      val schemaNode = nodeFactory.wrap(node)
      schemaNode.graph = graph
      schemaNode
    }
  }

  def filterRelations[START <: Node, RELATION <: Relation[START, END], END <: Node]
  (relations: Set[raw.Relation], relationFactory: RelationFactory[START, RELATION, END]): Set[RELATION] = {
    relations.filter(_.relationType == relationFactory.relationType).map(relationFactory.wrap)
  }

  def filterHyperRelations[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (nodes: Set[raw.Node], relations: Set[raw.Relation],
   hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END])
  : Set[HYPERRELATION] = {
    nodes.filter(_.labels.contains(hyperRelationFactory.label)).map { node =>
      val startRelation = relations.find(relation => relation.relationType == hyperRelationFactory.startRelationType && relation.endNode == node)
      val endRelation = relations.find(relation => relation.relationType == hyperRelationFactory.endRelationType && relation.startNode == node)
      hyperRelationFactory.wrap(startRelation.get, node, endRelation.get)
    }
  }
}

