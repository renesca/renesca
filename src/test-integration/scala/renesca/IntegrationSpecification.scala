package renesca

import org.specs2.mutable.Specification
import org.specs2.specification.{AfterExample, Step, Fragments}
import spray.can.Http.ConnectionAttemptFailedException

object IntegrationTestSetup  {
  val db = new DbService
  db.restService = new RestService("http://localhost:7474") // TODO: don't hardcode, configure in environment

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

  def cleanupDb() {
    db.batchQuery("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r")
  }
}

class IntegrationTestSetupSpec extends Specification {
  // Fails the whole run if testing db is not set up
  "Database should be available and empty" in {
    IntegrationTestSetup.testDbReady mustEqual true
  }
}

trait IntegrationSpecification extends Specification with AfterExample {
  sequential // Specs are also executed sequentially (build.sbt)
  if(!IntegrationTestSetup.testDbReady) skipAll

  // clean database after every Spec
  // override def map(fs: =>Fragments) = fs ^ Step(IntegrationTestSetup.cleanUpDb())

  // clean database after every example
  override protected def after = IntegrationTestSetup.cleanupDb()

  implicit val db = IntegrationTestSetup.db
}

