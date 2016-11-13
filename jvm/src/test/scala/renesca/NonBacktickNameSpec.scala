package renesca

import org.specs2.mutable.Specification

class NonBacktickNameSpec extends Specification {
  "NonBacktickName" >> {
    "disallow backticks in names" >> {
      NonBacktickName("foo`bar") must throwA[IllegalArgumentException]
      Label("foo`bar") must throwA[IllegalArgumentException]
      PropertyKey("foo`bar") must throwA[IllegalArgumentException]
      RelationType("foo`bar") must throwA[IllegalArgumentException]
    }
    "allow names without backticks" >> {
      NonBacktickName("foobar") must not(throwA[IllegalArgumentException])
      Label("foobar") must not(throwA[IllegalArgumentException])
      PropertyKey("foobar") must not(throwA[IllegalArgumentException])
      RelationType("foobar") must not(throwA[IllegalArgumentException])
    }
  }
}
