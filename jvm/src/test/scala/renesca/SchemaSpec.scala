package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import renesca.graph.{Label, RelationType}
import renesca.schema._
import renesca.{graph => raw}

@RunWith(classOf[JUnitRunner])
class SchemaSpec extends Specification with Mockito {

  object TheSchema {

    class TheGraph(val graph: raw.Graph = raw.Graph.empty) extends Graph {
      def abstractRelations = ???
      def hyperRelations = ???
      def nodes = ???
      def relations = ???
    }

    class TheNode(val rawItem: raw.Node) extends Node {
      val label = TheNode.label
      val labels = TheNode.labels
    }

    class TheRelation(val startNode: Node, val rawItem: raw.Relation, val endNode: Node)
      extends Relation[Node, Node]

    class TheOtherRelation(val startNode: TheHyperRelation, val rawItem: raw.Relation, val endNode: TheNode)
      extends Relation[TheHyperRelation, TheNode]

    class StartHyperRelation(val startNode: TheNode, val rawItem: raw.Relation, val endNode: TheHyperRelation)
      extends Relation[TheNode, TheHyperRelation]

    class EndHyperRelation(val startNode: TheHyperRelation, val rawItem: raw.Relation, val endNode: TheNode)
      extends Relation[TheHyperRelation, TheNode]

    class TheHyperRelation(val rawItem: raw.Node)
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

    object TheRelation extends RelationFactory[Node, TheRelation, Node] {
      override def relationType: RelationType = "peter"
      override def wrap(relation: raw.Relation): TheRelation = {
        new TheRelation(TheNode.wrap(relation.startNode), relation, TheNode.wrap(relation.endNode))
      }
      def apply(startNode: Node, endNode: Node) = {
        new TheRelation(startNode, raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem), endNode)
      }
    }

    object TheOtherRelation extends RelationFactory[TheHyperRelation, TheOtherRelation, TheNode] {
      override def relationType: RelationType = "hans"
      override def wrap(relation: raw.Relation): TheOtherRelation = {
        new TheOtherRelation(TheHyperRelation.wrap(relation.startNode), relation, TheNode.wrap(relation.endNode))
      }
      def apply(startNode: TheHyperRelation, endNode: TheNode) = {
        new TheOtherRelation(startNode, raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem), endNode)
      }
    }

    object StartHyperRelation
      extends RelationFactory[TheNode, StartHyperRelation, TheHyperRelation] {
      override def relationType: RelationType = "startpeter"
      override def wrap(relation: raw.Relation): StartHyperRelation = {
        new StartHyperRelation(TheNode.wrap(relation.startNode), relation, TheHyperRelation.wrap(relation.endNode))
      }
      def apply(startNode: TheNode, endNode: TheHyperRelation) = {
        new StartHyperRelation(startNode, raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem), endNode)
      }
    }

    object EndHyperRelation
      extends RelationFactory[TheHyperRelation, EndHyperRelation, TheNode] {
      override def relationType: RelationType = "endpeter"
      override def wrap(relation: raw.Relation): EndHyperRelation = {
        new EndHyperRelation(TheHyperRelation.wrap(relation.endNode), relation, TheNode.wrap(relation.startNode))
      }
      def apply(startNode: TheHyperRelation, endNode: TheNode) = {
        new EndHyperRelation(startNode, raw.Relation.create(startNode.rawItem, relationType, endNode.rawItem), endNode)
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
    node.graph mustEqual graph.graph
  }

  "remove node from graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    graph.add(node, node2)
    graph.remove(node)

    graph.graph.nodes must contain(exactly(node2.rawItem))
    node.graph mustNotEqual graph.graph
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
    neighbours.head.rawItem mustEqual node2.rawItem
    neighboursReverse.head.rawItem mustEqual node.rawItem
    neighbours2.size mustEqual 1
    neighbours2.head.rawItem mustEqual node.rawItem
    successors.size mustEqual 1
    successors.head.rawItem mustEqual node2.rawItem
    predecessors.size mustEqual 1
    predecessors.head.rawItem mustEqual node.rawItem
  }

  "remove relation from graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    graph.add(node, node2, relation)
    graph.remove(relation)

    graph.graph.nodes must contain(exactly(node.rawItem, node2.rawItem))
    graph.graph.relations.size mustEqual 0
    node.graph mustEqual graph.graph
    node2.graph mustEqual graph.graph
  }

  "remove node of relation from graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    graph.add(node, node2, relation)
    graph.remove(node2)

    graph.graph.nodes must contain(exactly(node.rawItem))
    graph.graph.relations.size mustEqual 0
    node.graph mustEqual graph.graph
    node2.graph mustNotEqual graph.graph
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
    relation.graph mustEqual graph.graph
    relation.startNodeOpt.get.rawItem mustEqual node.rawItem
    relation.endNodeOpt.get.rawItem mustEqual node2.rawItem
    startHyperRelations.size mustEqual 1
    endHyperRelations.size mustEqual 1
    hyperRelations.size mustEqual 1
    neighbours.size mustEqual 1
    neighbours.head.rawItem mustEqual relation.rawItem
    neighboursReverse.head.rawItem mustEqual node.rawItem
    relationNeighbours.map(_.rawItem) must contain(exactly(node.rawItem, node2.rawItem))
    neighbours2.size mustEqual 1
    neighbours2.head.rawItem mustEqual relation.rawItem
    successors.size mustEqual 1
    successors.head.rawItem mustEqual node2.rawItem
    predecessors.size mustEqual 1
    predecessors.head.rawItem mustEqual node.rawItem
  }

  "remove hyperrelation from graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheHyperRelation(node, node2)
    graph.add(node, node2, relation)
    graph.remove(relation)

    graph.graph.nodes must contain(exactly(node.rawItem, node2.rawItem))
    graph.graph.relations.size mustEqual 0
    node.graph mustEqual graph.graph
    node2.graph mustEqual graph.graph
    relation.graph mustNotEqual graph.graph
  }

  "remove node of hyperrelation from graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheHyperRelation(node, node2)
    graph.add(node, node2, relation)
    graph.remove(node)

    graph.graph.nodes must contain(exactly(node2.rawItem))
    graph.graph.relations.size mustEqual 0
    node.graph mustNotEqual graph.graph
    node2.graph mustEqual graph.graph
    relation.graph mustNotEqual graph.graph
  }.pendingUntilFixed("removal of start/end node should trigger removal of hyperrelations")

  "add hyperrelation without start- and endnode" >> {
    val graph = new TheGraph
    val relation = TheHyperRelation()
    graph.add(relation)

    val hyperRelations = graph.hyperRelationsAs(TheHyperRelation)

    relation.startRelationOpt mustEqual None
    relation.endRelationOpt mustEqual None
    hyperRelations.head.rawItem mustEqual relation.rawItem
  }

  "correct start/end node accessors" in {
    val a = TheNode()
    val b = TheNode()
    val relation = TheRelation(a, b)

    relation.startNode mustEqual relation.startNodeOpt.get
    relation.endNode mustEqual relation.endNodeOpt.get
    relation.startNode.rawItem mustEqual a.rawItem
    relation.endNode.rawItem mustEqual b.rawItem
  }

  "set graph in filterNodes" in {
    val g = mock[raw.Graph]
    val filter = new Filter {override val graph = g }
    val node = TheNode()
    val filtered = filter.filterNodes(Seq(node.rawItem), TheNode)
    filtered.head.graph mustEqual g
  }

  "set graph in filterHyperRelations" in {
    val g = mock[raw.Graph]
    val filter = new Filter {override val graph = g }
    val node = TheHyperRelation()
    val filtered = filter.filterHyperRelations(Seq(node.rawItem), Nil, TheHyperRelation)
    filtered.head.graph mustEqual g
  }

  "node.relationsAs" in {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    val relation2 = TheRelation(node2, node)
    graph.add(node, node2, relation, relation2)

    node.relationsAs(TheRelation) must contain(exactly(relation, relation2))
    node.inRelationsAs(TheRelation) must contain(exactly(relation2))
    node2.outRelationsAs(TheRelation) must contain(exactly(relation2))
  }

  "node.relationsAs (hyperrelations)" in {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheHyperRelation(node, node2)
    val relation2 = TheHyperRelation(node2, node)
    graph.add(node, node2, relation, relation2)

    node.relationsAs(TheHyperRelation) must contain(exactly(relation, relation2))
    node.inRelationsAs(TheHyperRelation) must contain(exactly(relation2))
    node2.outRelationsAs(TheHyperRelation) must contain(exactly(relation2))
  }

  "node equality" in {
    val node = TheNode()
    val node2 = TheNode.wrap(node.rawItem)
    val node3 = TheNode()

    node.equals(node2) mustEqual true
    node.equals(node3) mustEqual false
    node.equals("hi") mustEqual false
  }

  "relation equality" in {
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    val relation1 = TheRelation.wrap(relation.rawItem)
    val relation2 = TheRelation(node, node2)

    relation.equals(relation1) mustEqual true
    relation.equals(relation2) mustEqual false
    relation.equals("hi") mustEqual false
  }

  "hyperrelation equality" in {
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheHyperRelation(node, node2)
    val relation1 = TheHyperRelation.wrap(relation.rawItem)
    val relation2 = TheHyperRelation(node, node2)

    relation.equals(relation1) mustEqual true
    relation.equals(relation2) mustEqual false
    relation.equals("hi") mustEqual false
  }

  "keep graph when wrapping relation" in {
    val schema = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    schema.add(node, node2, relation)

    node.graph mustEqual schema.graph
    node2.graph mustEqual schema.graph
    node.graph mustEqual node.relationsAs(TheRelation).head.startNode.graph
    node2.graph mustEqual node.relationsAs(TheRelation).head.endNode.graph
  }

  "keep graph when wrapping hyperrelation" in {
    val schema = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheHyperRelation(node, node2)
    schema.add(node, node2, relation)

    node.graph mustEqual schema.graph
    node2.graph mustEqual schema.graph
    relation.graph mustEqual schema.graph
    relation.graph mustEqual node.relationsAs(StartHyperRelation).head.endNode.graph
    node.graph mustEqual node.relationsAs(TheHyperRelation).head.startNodeOpt.get.graph
    node2.graph mustEqual node.relationsAs(TheHyperRelation).head.endNodeOpt.get.graph
  }

  "recursively add start- and endnode of relation" in {
    val schema = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val hyperrelation = TheHyperRelation(node, node2)
    val node3 = TheNode()
    val relation = TheRelation(hyperrelation, node3)
    schema.add(relation)

    schema.graph.nodes must contain(exactly(node.rawItem, node2.rawItem, hyperrelation.rawItem, node3.rawItem))
    Seq(node, node2, hyperrelation, node3).map(_.graph).distinct must contain(exactly(schema.graph))
  }

  "hyperrelation keeps start- and endnode if wrapped with relation" in {
    val schema = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val hyperrelation = TheHyperRelation(node, node2)
    val node3 = TheNode()
    val relation = TheOtherRelation(hyperrelation, node3)
    schema.add(relation)

    val conHyper = node3.inRelationsAs(TheOtherRelation).head.startNode
    conHyper.startNodeOpt.get.rawItem mustEqual node.rawItem
    conHyper.endNodeOpt.get.rawItem mustEqual node2.rawItem
  }.pendingUntilFixed

  "hyperrelation keeps start- and endnode if wrapped as neighbour" in {
    val schema = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val hyperrelation = TheHyperRelation(node, node2)
    val node3 = TheNode()
    val relation = TheOtherRelation(hyperrelation, node3)
    schema.add(relation)

    val conHyper = node3.neighboursAs(TheHyperRelation).head
    conHyper.startNodeOpt.get.rawItem mustEqual node.rawItem
    conHyper.endNodeOpt.get.rawItem mustEqual node2.rawItem
  }.pendingUntilFixed
}

