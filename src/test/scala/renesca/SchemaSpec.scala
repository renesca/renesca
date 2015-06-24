package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.{graph => raw}
import renesca.graph.{RelationType, Label}
import renesca.schema._

@RunWith(classOf[JUnitRunner])
class SchemaSpec extends Specification with Mockito {

  object TheSchema {
    class TheGraph(val graph: raw.Graph = raw.Graph.empty) extends Graph

    class TheNode(val node: raw.Node) extends Node {
      val label = TheNode.label
      val labels = TheNode.labels
    }

    class TheRelation(val startNode: TheNode, val relation: raw.Relation, val endNode: TheNode)
      extends Relation[TheNode, TheNode]

    class StartHyperRelation(val startNode: TheNode, val relation: raw.Relation, val endNode: TheHyperRelation)
      extends Relation[TheNode, TheHyperRelation]

    class EndHyperRelation(val startNode: TheHyperRelation, val relation: raw.Relation, val endNode: TheNode)
      extends Relation[TheHyperRelation, TheNode]

    class TheHyperRelation(val node: raw.Node)
      extends HyperRelation[TheNode, StartHyperRelation, TheHyperRelation, EndHyperRelation, TheNode] {
      val label = TheHyperRelation.label
      val labels = TheHyperRelation.labels
    }

    object TheNode extends NodeFactory[TheNode] {
      val label: Label = "TheNode"
      val labels: Set[Label] = Set("TheNode")
      override def wrap(node: raw.Node): TheNode = {
        new TheNode(node)
      }
      def apply() = {
        wrap(raw.Node.create(Set(label)))
      }
    }

    object TheRelation extends RelationFactory[TheNode, TheRelation, TheNode] {
      override def relationType: RelationType = "peter"
      override def wrap(relation: raw.Relation): TheRelation = {
        new TheRelation(TheNode.wrap(relation.startNode), relation, TheNode.wrap(relation.endNode))
      }
      def apply(startNode: TheNode, endNode: TheNode) = {
        new TheRelation(startNode, raw.Relation.create(startNode.node, relationType, endNode.node), endNode)
      }
    }

    object StartHyperRelation
      extends RelationFactory[TheNode, StartHyperRelation, TheHyperRelation] {
      override def relationType: RelationType = "startpeter"
      override def wrap(relation: raw.Relation): StartHyperRelation = {
        new StartHyperRelation(TheNode.wrap(relation.startNode), relation, TheHyperRelation.wrap(relation.endNode))
      }
      def apply(startNode: TheNode, endNode: TheHyperRelation) = {
        new StartHyperRelation(startNode, raw.Relation.create(startNode.node, relationType, endNode.node), endNode)
      }
    }

    object EndHyperRelation
      extends RelationFactory[TheHyperRelation, EndHyperRelation, TheNode] {
      override def relationType: RelationType = "endpeter"
      override def wrap(relation: raw.Relation): EndHyperRelation = {
        new EndHyperRelation(TheHyperRelation.wrap(relation.endNode), relation, TheNode.wrap(relation.startNode))
      }
      def apply(startNode: TheHyperRelation, endNode: TheNode) = {
        new EndHyperRelation(startNode, raw.Relation.create(startNode.node, relationType, endNode.node), endNode)
      }
    }

    object TheHyperRelation
      extends HyperRelationFactory[TheNode, StartHyperRelation, TheHyperRelation, EndHyperRelation, TheNode] {
      val label: Label = "TheHyperRelation"
      val labels: Set[Label] = Set("TheHyperRelation")
      def startRelationType = StartHyperRelation.relationType
      def endRelationType = EndHyperRelation.relationType

      override def wrap(middleNode: raw.Node): TheHyperRelation = {
        new TheHyperRelation(middleNode)
      }

      override def wrap(startRelation: raw.Relation, middleNode: raw.Node, endRelation: raw.Relation): TheHyperRelation = {
        val middle = wrap(middleNode)
        middle._startRelation = StartHyperRelation.wrap(startRelation)
        middle._endRelation = EndHyperRelation.wrap(endRelation)
        middle
      }

      def apply(): TheHyperRelation = {
        new TheHyperRelation(raw.Node.create(Set(label)))
      }

      def apply(startNode: TheNode, endNode: TheNode): TheHyperRelation = {
        val middle = apply()
        middle._startRelation = StartHyperRelation(startNode, middle)
        middle._endRelation = EndHyperRelation(middle, endNode)
        middle
      }

    }
  }

  import TheSchema._

  "use node without graph" >> {
    val node = TheNode()
    val nodes = node.neighboursAs(TheNode)

    nodes.size mustEqual 0
  }

  "add node to graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    graph.add(node)
    val nodes = node.neighboursAs(TheNode)

    nodes.size mustEqual 0
  }

  "add node to two graphs and fail" >> {
    val graph = new TheGraph
    val graph2 = new TheGraph
    val node = TheNode()
    graph.add(node)
    graph2.add(node) must throwA[IllegalStateException]
  }

  "add nodes and relation to graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    graph.add(node, node2, relation)

    val nodes = graph.nodesAs(TheNode)
    val relations = graph.relationsAs(TheRelation)
    val hyperRelations = graph.hyperRelationsAs(TheHyperRelation)

    val neighbours = node.neighboursAs(TheNode)
    val neighbours2 = node2.neighboursAs(TheNode)
    val successors = node.successorsAs(TheNode, TheRelation)
    val predecessors = node2.predecessorsAs(TheNode, TheRelation)
    val neighboursReverse = neighbours.head.neighboursAs(TheNode)

    nodes.size mustEqual 2
    relation.relationType mustEqual RelationType("peter")
    relations.size mustEqual 1
    hyperRelations.size mustEqual 0
    neighbours.size mustEqual 1
    neighbours.head.node mustEqual node2.node
    neighboursReverse.head.node mustEqual node.node
    neighbours2.size mustEqual 1
    neighbours2.head.node mustEqual node.node
    successors.size mustEqual 1
    successors.head.node mustEqual node2.node
    predecessors.size mustEqual 1
    predecessors.head.node mustEqual node.node
  }

  "add nodes and hyperrelation to graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheHyperRelation(node, node2)
    graph.add(node, node2, relation)

    val nodes = graph.nodesAs(TheNode)
    val startHyperRelations = graph.relationsAs(StartHyperRelation)
    val endHyperRelations = graph.relationsAs(EndHyperRelation)
    val hyperRelations = graph.hyperRelationsAs(TheHyperRelation)

    val neighbours = node.neighboursAs(TheHyperRelation)
    val neighboursReverse = neighbours.head.neighboursAs(TheNode)
    val relationNeighbours = relation.neighboursAs(TheNode)
    val neighbours2 = node2.neighboursAs(TheHyperRelation)
    val successors = node.successorsAs(TheNode, TheHyperRelation)
    val predecessors = node2.predecessorsAs(TheNode, TheHyperRelation)

    nodes.size mustEqual 2
    startHyperRelations.size mustEqual 1
    endHyperRelations.size mustEqual 1
    hyperRelations.size mustEqual 1
    neighbours.size mustEqual 1
    neighbours.head.node mustEqual relation.node
    neighboursReverse.head.node mustEqual node.node
    relationNeighbours.map(_.node) must contain(exactly(node.node, node2.node))
    neighbours2.size mustEqual 1
    neighbours2.head.node mustEqual relation.node
    successors.size mustEqual 1
    successors.head.node mustEqual node2.node
    predecessors.size mustEqual 1
    predecessors.head.node mustEqual node.node
  }

  "add hyperrelation without start- and endnode" >> {
    val graph = new TheGraph
    val relation = TheHyperRelation()
    // graph.add(relation)
    graph.graph.nodes += relation.node

    val hyperRelations = graph.hyperRelationsAs(TheHyperRelation)

    relation.startRelation mustEqual null
    relation.endRelation mustEqual null
    hyperRelations.head.node mustEqual relation.node
  }
}

