package renesca

import renesca.graph._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.parameter.implicits._

@RunWith(classOf[JUnitRunner])
class TransactionDbSpec extends IntegrationSpecification {
  "Transactions" should {
    def newTransaction = {
      val transaction = new Transaction
      transaction.restService = db.restService // TODO injection
      transaction
    }
    "singleRequest rollback on errors" in {
      db.query("create (n)", "blabla illegal query") must throwA[RuntimeException]

      val graph = db.queryGraph("match n return n")
      graph.isEmpty mustEqual true
    }

    "openTransaction rollback on errors" in {
      val transaction = newTransaction

      transaction.query(
        "create (n)",
        "blabla illegal query"
      ) must throwA[RuntimeException]

      val graph = db.queryGraph("match n return n")
      graph.isEmpty mustEqual true
    }

    "Manual transaction success" in {
      val transaction = newTransaction

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      transaction.query(Query("match (n) where id(n) = {id} delete n", Map("id" -> first.id)))
      transaction.commit()

      val result = db.queryGraph("match n return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist graph changes in transaction" in {
      val transaction = newTransaction

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      graph.nodes -= first
      transaction.persistChanges(graph)
      transaction.commit()

      val result = db.queryGraph("match n return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist and commit graph changes in transaction (delete node)" in {
      val transaction = newTransaction

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      graph.nodes -= first
      transaction.commit.persistChanges(graph)

      val result = db.queryGraph("match n return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist and commit graph changes in transaction (add node)" in {
      val transaction = newTransaction

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val node = Node.local
      graph.nodes += node
      transaction.commit.persistChanges(graph)

      val result = db.queryGraph("match n return n")
      result.nodes must contain(node)
    }

    "Submit last query on commit" in {
      val transaction = newTransaction

      transaction.query("create (n) return n")
      transaction.commit.queryGraphs("create (y) return y")

      val result = db.queryGraph("match n return n")
      result.nodes must haveSize(2)
    }

    "only commit with a query" in {
      val transaction = newTransaction

      transaction.commit.queryGraphs("create n return n")

      val result = db.queryGraph("match n return n")
      result.nodes must haveSize(1)
    }

    "error on openTransaction is thrown by Transaction" in {
      val transaction = newTransaction

      transaction.query("boom") must throwA[RuntimeException]

      transaction.isValid mustEqual false
      val result = db.queryGraph("match n return n")
      result.nodes must haveSize(0)
    }

    "error on resumeTransaction is thrown by Transaction" in {
      val transaction = newTransaction

      transaction.query("create n return n")
      transaction.query("boom") must throwA[RuntimeException]

      transaction.isValid mustEqual false
      val result = db.queryGraph("match n return n")
      result.nodes must haveSize(0)
    }

    "do a manual rollback" in {
      val transaction = newTransaction

      transaction.query("create n return n")
      transaction.rollback()

      transaction.isValid mustEqual false
      val result = db.queryGraph("match n return n")
      result.nodes must haveSize(0)
    }

    "be isolated" in {
      val transaction = newTransaction

      transaction.query("create n return n")
      val graph = db.queryGraph("match n return n")
      transaction.rollback()

      graph.nodes must haveSize(0)
    }
  }
}
