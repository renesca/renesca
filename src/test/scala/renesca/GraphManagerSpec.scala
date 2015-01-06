package renesca

import org.specs2.mutable.Specification
import renesca.graph.NodeSetProperty
import json.Value._

class GraphManagerSpec extends Specification {
  "GraphManager" should {
    "convert NodeSetProperty to Query" in {
      val change = NodeSetProperty(1, "Key", "Value")
      val query = GraphManager.graphChangeToQuery(change)
      query mustEqual Query("match (n) where id(n) = {id} set n.Key = {value}", Map("id" -> 1, "value" -> "Value"))
    }
  }

}
