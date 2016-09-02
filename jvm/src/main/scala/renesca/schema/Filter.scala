package renesca.schema

import renesca.{graph => raw}

trait Filter {
  def graph: raw.Graph

  def filterNodes[T <: Node](nodes: Seq[raw.Node], nodeFactory: NodeFactory[T]): Seq[T] = {
    nodes.filter(_.labels.contains(nodeFactory.label)).map { node =>
      val schemaNode = nodeFactory.wrap(node)
      schemaNode.graphOption = Some(graph)
      schemaNode
    }
  }

  def filterRelations[START <: Node, RELATION <: Relation[START, END], END <: Node]
  (relations: Seq[raw.Relation], relationFactory: RelationFactory[START, RELATION, END]): Seq[RELATION] = {
    relations.filter(_.relationType == relationFactory.relationType).map { rel =>
      val schemaRel = relationFactory.wrap(rel)
      schemaRel.startNode.graphOption = Some(graph)
      schemaRel.endNode.graphOption = Some(graph)
      schemaRel
    }
  }

  def filterHyperRelations[
  START <: Node,
  STARTRELATION <: Relation[START, HYPERRELATION],
  HYPERRELATION <: HyperRelation[START, STARTRELATION, HYPERRELATION, ENDRELATION, END],
  ENDRELATION <: Relation[HYPERRELATION, END],
  END <: Node]
  (nodes: Seq[raw.Node], relations: Seq[raw.Relation],
   hyperRelationFactory: HyperRelationFactory[START, STARTRELATION, HYPERRELATION, ENDRELATION, END])
  : Seq[HYPERRELATION] = {
    nodes.filter(_.labels.contains(hyperRelationFactory.label)).map { node =>
      val startRelation = relations.find(relation => relation.relationType == hyperRelationFactory.startRelationType && relation.endNode == node)
      val endRelation = relations.find(relation => relation.relationType == hyperRelationFactory.endRelationType && relation.startNode == node)
      // The Start- and EndRelation might not be part of the graph
      val schemaNode = if(startRelation.isEmpty || endRelation.isEmpty)
        hyperRelationFactory.wrap(node)
      else
        hyperRelationFactory.wrap(startRelation.get, node, endRelation.get)
      schemaNode.graphOption = Some(graph)
      schemaNode.startNodeOpt.foreach(_.graphOption = Some(graph))
      schemaNode.endNodeOpt.foreach(_.graphOption = Some(graph))
      schemaNode
    }
  }
}

