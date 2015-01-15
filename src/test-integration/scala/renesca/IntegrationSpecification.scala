package renesca

import org.specs2.mutable.Specification
import spray.can.Http.ConnectionAttemptFailedException

object IntegrationTestSetup  {
  val db = new DbService
  db.restService = new RestService

  lazy val testDbReady = {
    lazy val (dbServerIsAvailable, dbIsEmpty, error) = {
      try {
        val graph = db.queryGraph("MATCH (n) RETURN n LIMIT 1")
        (true, graph.isEmpty, None)
      }
      catch{
        case e:Exception =>
          (false, false, Some(e.getMessage))
      }
    }

    val ready = dbServerIsAvailable && dbIsEmpty

    if(!ready) {
      println("")
      if(!dbServerIsAvailable) {
        // TODO: log error
        println("Cannot connect to Database Server.")

      } else if(!dbIsEmpty) {
        println("Test database is not empty.")
      }
      for(errorMessage <- error) println("Exception: " + errorMessage)
      println("Skipping all integration tests.")
      println("")
    }

    ready
  }
}


trait IntegrationSpecification extends Specification {
  sequential // Specs are also executed sequentially (build.sbt)
  if(!IntegrationTestSetup.testDbReady) skipAll

  //TODO: clear database when done
}

class IntegrationTestSetupSpec extends Specification {
  "Database should be available and empty" in {
    IntegrationTestSetup.testDbReady mustEqual true
  }
}
