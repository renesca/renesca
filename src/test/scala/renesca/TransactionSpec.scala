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

      tx.restService.openTransaction(jsonRequest) returns ((
        TransactionId("1"),
        json.Response(
          commit = Some("http://localhost:7474/db/data/transaction/1/commit"),
          transaction = Some(json.Transaction("Thu, 29 Jan 2015 11:56:08 +0000"))
        )))

      tx.queryGraph(statement)
      tx.commit()

      there was one(tx.restService).openTransaction(jsonRequest)
      there was one(tx.restService).commitTransaction(TransactionId("1"))

      there was no(tx.restService).resumeTransaction(TransactionId("1"),jsonRequest)
    }
  }

  "open transaction on first, resume on second request" in {
    val tx = new Transaction
    tx.restService = mock[RestService]
    val statement = "match n return n"
    val jsonRequest = json.Request(List(json.Statement(Query(statement), List("graph"))))

    tx.restService.openTransaction(jsonRequest) returns ((
      TransactionId("1"),
      json.Response(
        commit = Some("http://localhost:7474/db/data/transaction/1/commit"),
        transaction = Some(json.Transaction("Thu, 29 Jan 2015 11:56:08 +0000"))
      )))

    tx.restService.resumeTransaction(TransactionId("1"),jsonRequest) returns json.Response(
        commit = Some("http://localhost:7474/db/data/transaction/1/commit"),
        transaction = Some(json.Transaction("Thu, 29 Jan 2015 11:56:08 +0000"))
      )

    tx.queryGraph(statement)
    tx.queryGraph(statement)
    tx.commit()

    there was one(tx.restService).openTransaction(jsonRequest)
    there was one(tx.restService).resumeTransaction(TransactionId("1"),jsonRequest)
    there was one(tx.restService).commitTransaction(TransactionId("1"))
  }

  "open transaction on commit" in {
    val tx = new Transaction
    tx.restService = mock[RestService]
    val statement = "match n return n"
    val jsonRequest = json.Request(List(json.Statement(Query(statement), List("graph"))))

    tx.restService.singleRequest(jsonRequest) returns json.Response()

    tx.commit(statement)

    there was one(tx.restService).singleRequest(jsonRequest)
    
    there was no(tx.restService).openTransaction(jsonRequest)
    there was no(tx.restService).commitTransaction(TransactionId("1"))
    there was no(tx.restService).resumeTransaction(TransactionId("1"),jsonRequest)
  }

}

