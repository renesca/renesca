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

  class TheGraph(val graph: raw.Graph = raw.Graph.empty) extends Graph

  class TheNode(val node: raw.Node) extends Node {
    val label = TheNode.label
    val labels = TheNode.labels
  }

  class TheRelation(val startNode: TheNode, val relation: raw.Relation, val endNode: TheNode) extends Relation[TheNode, TheNode]

  object TheNode extends NodeFactory[TheNode] {
    val label: Label = "TheNode"
    val labels: Set[Label] = Set("TheNode")
    override def wrap(node: raw.Node): TheNode = new TheNode(node)
    def apply() = wrap(raw.Node.create(Set(label)))
  }

  object TheRelation extends RelationFactory[TheNode, TheRelation, TheNode] {
    override def relationType: RelationType = "peter"
    override def wrap(relation: raw.Relation): TheRelation = new TheRelation(TheNode.wrap(relation.startNode), relation, TheNode.wrap(relation.endNode))
    def apply(startNode: TheNode, endNode: TheNode) = new TheRelation(startNode, raw.Relation.create(startNode.node, relationType, endNode.node), endNode)
  }

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

  "add node and relation to graph" >> {
    val graph = new TheGraph
    val node = TheNode()
    val node2 = TheNode()
    val relation = TheRelation(node, node2)
    graph.add(node, node2, relation)

    val neighbours = node.neighboursAs(TheNode)
    val neighbours2 = node2.neighboursAs(TheNode)
    val successors = node.successorsAs(TheNode, TheRelation)
    val predecessors = node2.predecessorsAs(TheNode, TheRelation)

    val neighboursReverse = neighbours.head.neighboursAs(TheNode)

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
}

