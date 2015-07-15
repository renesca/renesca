package renesca.schema

// schema.Graph is a typed wrapper around the untyped renesca Graph.
// It provides methods to filter Nodes, Relations and HyperRelations by labels/relationTypes (passed in by factories).
// It also provides an add-method to insert the underlying Nodes/Relations contained in the corresponding wrappers.
// These additions are automatically tracked as GraphChanges by the underlying graph.

import renesca.graph.Path
import renesca.{graph => raw}


//TODO: implicits from Graph to raw.Graph
trait Graph extends Filter {
  def nodes: Set[_ <: Node]
  def relations: Set[_ <: Relation[_,_]]
  def abstractRelations: Set[_ <: AbstractRelation[_,_]]
  def hyperRelations: Set[_ <: HyperRelation[_,_,_,_,_]]

  def nodesAs[T <: Node](nodeFactory: NodeFactory[T]) = {
    filterNodes(graph.nodes.toSet, nodeFactory)
  }

  def relationsAs[RELATION <: Relation[START, END], START <: Node, END <: Node]
  (relationFactory: RelationFactory[START, RELATION, END]) = {
    filterRelations(graph.relations.toSet, relationFactory)
  }

  def hyperRelationsAs[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END]) = {
    filterHyperRelations(graph.nodes.toSet, graph.relations.toSet, hyperRelationFactory)
  }

  def add(schemaItems: Item*) {
    schemaItems.foreach {
      case hyperRelation: HyperRelation[_, _, _, _, _] =>
        graph.nodes += hyperRelation.rawItem
        hyperRelation.startRelationOpt.foreach(graph.relations += _.rawItem)
        hyperRelation.endRelationOpt.foreach(graph.relations += _.rawItem)
        hyperRelation.rawPath.foreach(graph += _)
        hyperRelation.graphOption = Some(graph)

      case relation: Relation[_, _] =>
        graph.relations += relation.rawItem

      case schemaNode: Node =>
        graph.nodes += schemaNode.rawItem
        schemaNode.graphOption = Some(graph)
    }
  }

  def remove(schemaItems: Item*) {
    schemaItems.foreach {
      case hyperRelation: HyperRelation[_, _, _, _, _] =>
        graph.nodes -= hyperRelation.rawItem
        hyperRelation.startRelationOpt.foreach(graph.relations -= _.rawItem)
        hyperRelation.endRelationOpt.foreach(graph.relations -= _.rawItem)
        hyperRelation.graphOption = None

      case relation: Relation[_, _] =>
        graph.relations -= relation.rawItem

      case schemaNode: Node =>
        graph.nodes -= schemaNode.rawItem
        schemaNode.graphOption = None
    }
  }
}


