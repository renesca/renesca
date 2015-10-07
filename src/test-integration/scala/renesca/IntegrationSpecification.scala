package renesca

import akka.util.Timeout
import org.specs2.execute.{AsResult, Result, ResultExecution, Skipped}
import org.specs2.mutable.Specification
import org.specs2.specification._
import spray.http.BasicHttpCredentials

import scala.concurrent.duration._

case class DbState(serverAvailable: Boolean, dbEmpty: Boolean, errorMsg: Option[String] = None) {
  def isAvailableAndEmpty = serverAvailable && dbEmpty
  def notReadyMessage: String = {
    if(!isAvailableAndEmpty) {
      (if(!serverAvailable)
         "Cannot connect to Database Server."
       else if(!dbEmpty) {
        "Test database is not empty."
      }) + errorMsg.map(" (" + _ + ")").getOrElse("")
    } else ""
  }
}

object IntegrationTestSetup {
  val testDb = new DbService
  testDb.restService = new RestService(// TODO: don't hardcode, configure in environment
    server = "http://localhost:7474",
    timeout = Timeout(60.seconds), // timeout needs to be longer than Neo4j transaction timeout (currently 3 seconds)
    credentials = Some(BasicHttpCredentials("neo4j", "testingpw"))
  )

  def dbState = {
    try {
      val graph = testDb.queryGraph("MATCH (n) RETURN n LIMIT 1")
      DbState(serverAvailable = true, dbEmpty = graph.isEmpty)
    }
    catch {
      case e: Exception =>
        DbState(serverAvailable = false, dbEmpty = false, Some(s"Exception: ${ e.getMessage }"))
    }
  }

  def dbIsReady = dbState.isAvailableAndEmpty

  val cleanupFailedMsg = "WARNING: Database cleanup failed on first try. Is there an unfinished transaction?"
  def cleanupDb() = {
    def deleteEverything() { testDb.query("MATCH (n) OPTIONAL MATCH (n)-[r]-() DELETE n,r") }

    try {
      deleteEverything() // fails if there is an unfinished Transaction
      true
    } catch {
      case e: Exception =>
        deleteEverything() // fails if requestTimeout <= neo4jTransactionTimeout
        false
    }
  }

}

class IntegrationTestSetupSpec extends Specification {
  // Fails the whole run if testing db is not set up
  "Database should be available and empty" in {
    IntegrationTestSetup.dbIsReady mustEqual true
  }
}

trait IntegrationSpecification extends Specification with AroundEach {
  sequential
  // Specs are also executed sequentially (build.sbt)

  // check for clean database before,
  // and clean database after every example
  def around[T: AsResult](t: => T): Result = {
    import IntegrationTestSetup._
    val db = dbState
    if(db.isAvailableAndEmpty) {
      val result = ResultExecution.execute(AsResult(t))
      if(cleanupDb())
        result
      else {
        // cleanUp succeeded with warning
        if(result.isFailure)
          result.mapMessage(oldMessage =>
            s"$cleanupFailedMsg\n$oldMessage")
        else
          result.mapExpected(_ => s"    $cleanupFailedMsg")
      }
    } else {
      new Skipped(s"\n    WARNING: ${ db.notReadyMessage }")
    }
  }

  implicit val db = IntegrationTestSetup.testDb
}

