package renesca

import org.specs2.mock._
import org.specs2.mutable._
import renesca.graph.Graph
import concurrent.{Future, Await}
import concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class TransactionSpec extends Specification with Mockito {

  val statement = "match n return n"
  val jsonRequest = json.Request(List(json.Statement(statement, List("graph"))))
  val jsonRequestWithoutResult = json.Request(List(json.Statement(statement)))
  val jsonResponse = json.Response(
    commit = Some("http://localhost:7474/db/data/transaction/1/commit"),
    transaction = Some(json.Transaction("Thu, 29 Jan 2015 11:56:08 +0000"))
  )
  val jsonResponseWithError = json.Response(
    errors = List(json.Error(
      "Neo.ClientError.Statement.InvalidSyntax",
      "Invalid input 'T': expected <init> (line 1, column 1)\n\"This is not a valid Cypher Statement.\"\n ^"
    ))
  )
  val transactionResponse = (TransactionId("1"), jsonResponse)

  def newTransaction = {
    val tx = new Transaction
    tx.restService = mock[RestService].smart

    tx.restService.singleRequest(jsonRequest) returns Future.successful(jsonResponse)
    tx.restService.openTransaction(jsonRequest) returns Future.successful(transactionResponse)
    tx.restService.openTransaction(jsonRequestWithoutResult) returns Future.successful(transactionResponse)
    tx.restService.resumeTransaction(TransactionId("1"), jsonRequest) returns Future.successful(jsonResponse)
    tx.restService.resumeTransaction(TransactionId("1"), jsonRequestWithoutResult) returns Future.successful(jsonResponse)
    tx.restService.commitTransaction(any, any) returns Future.successful(json.Response())

    tx
  }

  "Transaction" >> {
    "open transaction on first request" >> {
      val tx = newTransaction

      val result = for (
        _ <- tx.query(statement);
        r <- tx.commit()
      ) yield r
      Await.ready(result, 10 seconds)

      there was one(tx.restService).openTransaction(jsonRequestWithoutResult)
      there was one(tx.restService).commitTransaction(TransactionId("1"))
      there was no(tx.restService).resumeTransaction(TransactionId("1"), jsonRequest)
    }
  }

  "open transaction on first, resume on second request" >> {
    val tx = newTransaction

    Await.ready(tx.query(statement), 10 seconds)
    Await.ready(tx.query(statement), 10 seconds)
    Await.ready(tx.commit(), 10 seconds)

    there was one(tx.restService).openTransaction(jsonRequestWithoutResult)
    there was one(tx.restService).resumeTransaction(TransactionId("1"), jsonRequestWithoutResult)
    there was one(tx.restService).commitTransaction(TransactionId("1"))
  }

  "open transaction on commit" >> {
    val tx = newTransaction

    tx.commit.queryGraphs(statement)

    there was one(tx.restService).singleRequest(jsonRequest)
    there was no(tx.restService).openTransaction(jsonRequest)
    there was no(tx.restService).commitTransaction(TransactionId("1"))
    there was no(tx.restService).resumeTransaction(TransactionId("1"), jsonRequest)
  }

  "invalidate transaction on commit" >> {
    val tx = newTransaction

    tx.commit()

    tx.isValid mustEqual false
  }

  "invalidate transaction on commit with query" >> {
    val tx = newTransaction

    val result = tx.commit.queryGraphs(statement)
    Await.ready(result, 10 seconds)

    tx.isValid mustEqual false
  }

  "throw exception when using invalidated transaction" >> {
    val tx = newTransaction
    tx.invalidate()

    Await.result(tx.queryGraph(statement), 10 seconds) must throwA[RuntimeException]
    Await.result(tx.commit(), 10 seconds) must throwA[RuntimeException]
    Await.result(tx.commit.queryGraphs(statement), 10 seconds) must throwA[RuntimeException]
  }

  "error in transaction rollbacks transaction and throws exception" >> {
    val tx = new Transaction
    tx.restService = mock[RestService]
    tx.restService.openTransaction(jsonRequest) returns Future.successful((TransactionId("1"), jsonResponseWithError))

    val result = tx.queryGraph(statement)
    Await.result(result, 10 seconds) must throwA[RuntimeException]

    tx.isValid mustEqual false
    there was one(tx.restService).rollbackTransaction(TransactionId("1"))
  }

  "don't commit when doing nothing in transaction" >> {
    val tx = newTransaction
    tx.commit()

    there was no(tx.restService).singleRequest(any)
    there was no(tx.restService).openTransaction(any)
    there was no(tx.restService).commitTransaction(any, any)
    there was no(tx.restService).resumeTransaction(any, any)
    there was no(tx.restService).rollbackTransaction(any)
  }

  "rollback transaction on exception with enclosing syntax" >> {
    val dbService = new DbService

    var trans: Transaction = null
    var thrown: Exception = null
    val ex = new Exception("test")
    try {
      dbService.transaction { tx =>
        trans = tx
        throw ex
      }
    } catch {
      case e: Exception => thrown = e
    }

    // TODO: should whether it was commited or rollbacked
    trans.isValid mustEqual false
    thrown mustEqual ex
  }

  "transaction persistChanges should rollback on error" >> {
    //TODO: why does it not work with spy?
    var rollbacked = 0
    class MehTransaction extends Transaction {
      override val builder = mock[QueryBuilder]
      builder.generateQueries(Seq.empty) returns Right(Seq.empty)
      builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.failed(new Exception("meh"))
      override def rollback() = { rollbacked += 1; Future.successful(Unit) }
      override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
      override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
    }

    //      val transaction = spy(new MehTransaction)
    val transaction = new MehTransaction

    val graph = mock[Graph]
    graph.changes returns Nil

    val result = transaction.commit.persistChanges(graph)
    Await.result(result, 10 seconds) must throwAn[Exception](message = "meh")

    there was no(graph).clearChanges()
    rollbacked mustEqual 1
    //      there was one(transaction).rollback()
  }

  "transaction persistChanges should commit on success" >> {
    var committed = 0
    class MehTransaction extends Transaction {
      override val builder = mock[QueryBuilder]
      builder.generateQueries(Seq.empty) returns Right(Seq.empty)
      builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.successful(Unit)
      override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
      override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)

      override val commit = new CommitTransaction {
        override def apply() = { committed += 1; Future.successful(Unit) }
      }
    }

    val transaction = spy(new MehTransaction)

    val graph = mock[Graph]
    graph.changes returns Nil

    val result = transaction.commit.persistChanges(graph)
    Await.result(result, 10 seconds)

    there was one(graph).clearChanges()
    committed mustEqual 1
  }
}
