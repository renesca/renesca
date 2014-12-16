package renesca

import org.specs2.mutable._
import org.specs2.mock._

class GraphSpec extends Specification with Mockito {

  "Graph" should {
    "run a test" in {
      "Hello world" must have size(11)
    }

    "run a test with mock" in {
      class A {
        val a = 5
      }
      val mockA = mock[A]

      mockA.a returns 6

      mockA.a mustEqual 6
    }
  }
}
