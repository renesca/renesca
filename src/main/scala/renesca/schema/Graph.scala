package renesca.schema

// schema.Graph is a typed wrapper around the untyped renesca Graph.
// It provides methods to filter Nodes, Relations and HyperRelations by labels/relationTypes (passed in by factories).
// It also provides an add-method to insert the underlying Nodes/Relations contained in the corresponding wrappers.
// These additions are automatically tracked as GraphChanges by the underlying graph.

import renesca.{graph => raw}
import renesca.parameter.StringPropertyValue
import renesca.parameter.implicits._


trait Graph extends NodeFilter {
  def add(schemaItem: Item) {
    schemaItem match {
      case hyperRelation: HyperRelation[_, _, _, _, _] =>
        graph.nodes += hyperRelation.node
        graph.relations += hyperRelation.startRelation.relation
        graph.relations += hyperRelation.endRelation.relation

      case relation: Relation[_, _] =>
        graph.relations += relation.relation

      case schemaNode: Node =>
        graph.nodes += schemaNode.node
    }
  }

  def nodesAs[T <: Node](nodeFactory: NodeFactory[T]) = {
    filterNodes(graph.nodes.toSet, nodeFactory)
  }

  def relationsAs[RELATION <: Relation[STARTNODE, ENDNODE], STARTNODE <: Node, ENDNODE <: Node]
  (relationFactory: RelationFactory[STARTNODE, RELATION, ENDNODE]) = {
    filterRelations(graph.relations.toSet, relationFactory)
  }

  def hyperRelationsAs[
  STARTNODE <: Node,
  STARTRELATION <: Relation[STARTNODE, HYPERRELATION],
  HYPERRELATION <: HyperRelation[STARTNODE, STARTRELATION, HYPERRELATION, ENDRELATION, ENDNODE],
  ENDRELATION <: Relation[HYPERRELATION, ENDNODE],
  ENDNODE <: Node]
  (hyperRelationFactory: HyperRelationFactory[STARTNODE, STARTRELATION, HYPERRELATION, ENDRELATION, ENDNODE]) = {
    filterHyperRelations(graph.nodes.toSet, graph.relations.toSet, hyperRelationFactory)
  }

}


