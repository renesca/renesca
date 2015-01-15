package renesca

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.graph.Graph

@RunWith(classOf[JUnitRunner])
class GraphManagerSpec extends Specification with Mockito  with HttpMockServer {
  "GraphManager" should {
    "clear changes after persisting" in {
      val graphManager = new GraphManager
      graphManager.dbService = mock[DbService]


      val graph = mock[Graph]
      graph.changes returns Nil

      graphManager.persistChanges(graph)
      there was one(graph).clearChanges()
    }
  }
}
