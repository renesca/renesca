package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSpec extends IntegrationSpecification {

  "IntegrationTest" should {
    "run a Test" in {
      val graph = db.queryGraph("merge n-[r:REL]->m return n,r,m")
      graph.nodes must have size(2)
    }

    "run another Test" in {
      val graph = db.queryGraph("match n return n")
      graph.nodes must have size(0)
    }
  }

}
