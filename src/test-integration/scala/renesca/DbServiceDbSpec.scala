package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.json._

@RunWith(classOf[JUnitRunner])
class DbServiceDbSpec extends IntegrationSpecification {

  "DbService" should {

    "throw exception on Neo4j Error" in {
      db.batchQuery("this is invalid cypher syntax") must throwA[RuntimeException]
    }
  }

}

