package renesca

import renesca.graph._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.parameter.implicits._

@RunWith(classOf[JUnitRunner])
class TransactionDbSpec extends IntegrationSpecification {
  "Transactions" should {
    "be created by dbService" in {
      val tx = db.newTransaction()
      tx.restService must not beNull
    }

    "singleRequest rollback on errors" in {
      db.query("create (n)", "blabla illegal query") must throwA[RuntimeException]

      val graph = db.queryGraph("match (n) return n")
      graph.isEmpty mustEqual true
    }

    "openTransaction rollback on errors" in {
      val transaction = db.newTransaction()

      transaction.query(
        "create (n)",
        "blabla illegal query"
      ) must throwA[RuntimeException]

      val graph = db.queryGraph("match (n) return n")
      graph.isEmpty mustEqual true
    }

    "Manual transaction success" in {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      transaction.query(Query("match (n) where id(n) = {id} delete n", Map("id" -> first.origin.asInstanceOf[Id].id)))
      transaction.commit()

      val result = db.queryGraph("match (n) return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "work with enclosing syntax" in {
      var first, second: Node = null
      db.transaction { tx =>
        tx.query("create (n),(m)")
        val graph = tx.queryGraph("match (n) return n")
        first = graph.nodes.head
        second = graph.nodes.last
        tx.query(Query("match (n) where id(n) = {id} delete n", Map("id" -> first.origin.asInstanceOf[Id].id)))
      }

      val result = db.queryGraph("match (n) return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist graph changes in transaction" in {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      graph.nodes -= first
      transaction.persistChanges(graph)
      transaction.commit()

      val result = db.queryGraph("match (n) return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist and commit graph changes in transaction (delete node)" in {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      graph.nodes -= first
      transaction.commit.persistChanges(graph)

      val result = db.queryGraph("match (n) return n")
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist and commit graph changes in transaction - add node" in {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val node = Node.create
      graph.nodes += node
      transaction.commit.persistChanges(graph)

      val result = db.queryGraph("match (n) return n")
      result.nodes must contain(node)
    }

    "Persist and commit graph changes in transaction - add relation" in {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      val relation = Relation.create(first, "likes", second)
      graph.relations += relation
      transaction.commit.persistChanges(graph)

      val result = db.queryGraph("MATCH (n) OPTIONAL MATCH (n)-[r]-() return n,r")
      result.nodes must contain(first)
      result.nodes must contain(second)
      result.relations must contain(relation)
    }

    "Submit last query on commit" in {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      transaction.commit.queryGraphs("create (y) return y")

      val result = db.queryGraph("match (n) return n")
      result.nodes must haveSize(2)
    }

    "only commit with a query" in {
      val transaction = db.newTransaction()

      transaction.commit.queryGraphs("create (n) return n")

      val result = db.queryGraph("match (n) return n")
      result.nodes must haveSize(1)
    }

    "error on openTransaction is thrown by Transaction" in {
      val transaction = db.newTransaction()

      transaction.query("boom") must throwA[RuntimeException]

      transaction.isValid mustEqual false
      val result = db.queryGraph("match (n) return n")
      result.nodes must haveSize(0)
    }

    "error on resumeTransaction is thrown by Transaction" in {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      transaction.query("boom") must throwA[RuntimeException]

      transaction.isValid mustEqual false
      val result = db.queryGraph("match (n) return n")
      result.nodes must haveSize(0)
    }

    "throw error exception on transaction commit" in {
      db.queryGraph("""create (n:N)-[r:R]->(m)""")

      val transaction = db.newTransaction()
      transaction.query("""MATCH (n:N) DELETE n""") // invalid delete, because (n) has a relation
      transaction.commit() must throwA[RuntimeException]
    }

    "do a manual rollback" in {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      transaction.rollback()

      transaction.isValid mustEqual false
      val result = db.queryGraph("match (n) return n")
      result.nodes must haveSize(0)
    }

    "be isolated" in {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      val graph = db.queryGraph("match (n) return n")
      transaction.rollback()

      graph.nodes must haveSize(0)
    }
  }
}
