package renesca

import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.graph.{RelationType, Label}
import renesca.json.PropertyKey

@RunWith(classOf[JUnitRunner])
class NonBacktickNameSpec extends Specification {
  "NonBacktickName" should {
    "disallow backticks in names" in {
      Label("foo`bar") must throwA[IllegalArgumentException]
      PropertyKey("foo`bar") must throwA[IllegalArgumentException]
      RelationType("foo`bar") must throwA[IllegalArgumentException]
    }
    "allow names without backticks" in {
      Label("foobar") must not(throwA[IllegalArgumentException])
      PropertyKey("foobar") must not(throwA[IllegalArgumentException])
      RelationType("foo`bar") must throwA[IllegalArgumentException]
    }
  }
}
