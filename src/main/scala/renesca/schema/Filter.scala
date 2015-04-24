package renesca.schema

import renesca.{graph => raw}
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._

trait NodeFilter {
  def graph: raw.Graph

  def filterNodes[T <: Node](nodes: Set[raw.Node], nodeFactory: NodeFactory[T]): Set[T] = {
    nodes.filter(_.labels.contains(nodeFactory.label)).map { node =>
      val schemaNode = nodeFactory.create(node)
      schemaNode.graph = graph
      schemaNode
    }
  }

  def filterRelations[STARTNODE <: Node, RELATION <: Relation[STARTNODE, ENDNODE], ENDNODE <: Node]
  (relations: Set[raw.Relation], relationFactory: RelationFactory[STARTNODE, RELATION, ENDNODE]): Set[RELATION] = {
    relations.filter(_.relationType == relationFactory.relationType).map(relationFactory.create)
  }

  def filterHyperRelations[
  STARTNODE <: Node,
  STARTRELATION <: Relation[STARTNODE, HYPERRELATION],
  HYPERRELATION <: HyperRelation[STARTNODE, STARTRELATION, HYPERRELATION, ENDRELATION, ENDNODE],
  ENDRELATION <: Relation[HYPERRELATION, ENDNODE],
  ENDNODE <: Node]
  (nodes: Set[raw.Node], relations: Set[raw.Relation],
   hyperRelationFactory: HyperRelationFactory[STARTNODE, STARTRELATION, HYPERRELATION, ENDRELATION, ENDNODE])
  : Set[HYPERRELATION] = {
    nodes.filter(_.labels.contains(hyperRelationFactory.label)).map { node =>
      val startRelation = relations.find(relation => relation.relationType == hyperRelationFactory.startRelationType && relation.endNode == node)
      val endRelation = relations.find(relation => relation.relationType == hyperRelationFactory.endRelationType && relation.startNode == node)
      hyperRelationFactory.create(startRelation.get, node, endRelation.get)
    }
  }
}
