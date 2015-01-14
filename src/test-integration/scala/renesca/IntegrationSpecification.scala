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

    if(!ready) println("")
    if(!dbServerIsAvailable) {
      // TODO: log error
      println("Cannot connect to Database Server.")
      println("Skipping all integration tests.")

    } else if(!dbIsEmpty) {
      println("Testing database is not empty.")
      println("Skipping all integration tests.")
    }
    if(!ready) {
      for(errorMessage <- error) println("Exception: " + errorMessage)
      println("")
    }

    ready
  }
}


trait IntegrationSpecification extends Specification {
  if(!IntegrationTestSetup.testDbReady) skipAll
  //TODO: fail when all tests are skipped
  //TODO: clear database when done
}
