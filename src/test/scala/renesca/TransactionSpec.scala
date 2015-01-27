package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph.{Graph, Node, Relation}
import renesca.json.protocols.ResponseJsonProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class TransactionSpec extends Specification with Mockito {

  "Transaction" should {
    "open transaction on first request" in {
      val tx = new Transaction
      tx.restService = mock[RestService]
      val statement = "match n return n"
      val jsonRequest = json.Request(List(json.Statement(Query(statement), List("graph"))))
//      tx.restService.openTransaction() returns

      tx.queryGraph(statement)
      tx.commit()

//      there was one(tx.restService).openTransaction
//      there was no(tx.restService).resumeTransaction
//      there was one(tx.restService).commitTransaction
      true mustEqual true
    }.pendingUntilFixed
  }

  "open transaction on first, resume on second request" in todo

  "open transaction on commit" in todo

}

