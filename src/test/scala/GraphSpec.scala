package renesca

import org.specs2.mutable._

class GraphSpec extends Specification {

  "Graph" should {
    "run a test" in {
      "Hello world" must have size(11)
    }
    "run a test with mock" in {
      val mockSet = mock[Set]
      "Hello world" must have size(11)
    }
  }
}
