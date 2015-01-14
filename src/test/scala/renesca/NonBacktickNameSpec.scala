package renesca

import org.specs2.mutable.Specification
import renesca.graph.Label
import renesca.json.PropertyKey

class NonBacktickNameSpec extends Specification {
  "NonBacktickName" should {
    "disallow backticks in names" in {
      Label("foo`bar") must throwA[IllegalArgumentException]
      PropertyKey("foo`bar") must throwA[IllegalArgumentException]
    }
    "allow names without backticks" in {
      Label("foobar") must not(throwA[IllegalArgumentException])
      PropertyKey("foobar") must not(throwA[IllegalArgumentException])
    }
  }
}
