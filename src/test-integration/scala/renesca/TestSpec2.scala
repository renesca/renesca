package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TestSpec2 extends IntegrationSpecification {
  sequential

  "IntegrationTest" should {
    "run a Test" in {
      true mustEqual true
    }

    "run another Test" in {
      true mustEqual true
    }
  }

}

