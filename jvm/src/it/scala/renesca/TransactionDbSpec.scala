package renesca

import renesca.graph._
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import concurrent.Await
import concurrent.duration._
import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

@RunWith(classOf[JUnitRunner])
class TransactionDbSpec extends IntegrationSpecification {

  implicit def toJson[T: Encoder](x: T) = x.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  "Transactions" >> {
    "be created by dbService" >> {
      val tx = db.newTransaction()
      tx.restService must not beNull
    }

    "singleRequest rollback on errors" >> {
      db.query("create (n)", "blabla illegal query") must throwA[RuntimeException]

      val graph = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      graph.isEmpty mustEqual true
    }

    "openTransaction rollback on errors" >> {
      val transaction = db.newTransaction()

      transaction.query(
        "create (n)",
        "blabla illegal query"
      ) must throwA[RuntimeException]

      val graph = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      graph.isEmpty mustEqual true
    }

    "Manual transaction success" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = Await.result(transaction.queryGraph("match (n) return n"), 60 seconds)
      val List(first, second) = graph.nodes.toList
      transaction.query(Query("match (n) where id(n) = {id} delete n", Map("id" -> first.origin.asInstanceOf[Id].id)))
      transaction.commit()

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "work with enclosing syntax" >> {
      var first, second: Node = null
      db.transaction { tx =>
        tx.query("create (n),(m)")
        val graph = Await.result(tx.queryGraph("match (n) return n"), 60 seconds)
        first = graph.nodes.head
        second = graph.nodes.last
        tx.query(Query("match (n) where id(n) = {id} delete n", Map("id" -> first.origin.asInstanceOf[Id].id)))
      }

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "property change with enclosing syntax" >> {
      var node: Node = null
      db.transaction { tx =>
        tx.query("create (n)")
        val graph = Await.result(tx.queryGraph("match (n) return n"), 60 seconds)
        node = graph.nodes.head
        node.properties("tut es?") = "ja"
        tx.persistChanges(graph)
      }

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes.head.properties("tut es?") mustEqual "ja"
    }

    "Persist graph changes in transaction" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = Await.result(transaction.queryGraph("match (n) return n"), 60 seconds)
      val List(first, second) = graph.nodes.toList
      graph.nodes -= first
      transaction.persistChanges(graph)
      transaction.commit()

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist and commit graph changes in transaction (delete node)" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = Await.result(transaction.queryGraph("match (n) return n"), 60 seconds)
      val List(first, second) = graph.nodes.toList
      graph.nodes -= first
      transaction.commit.persistChanges(graph)

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must not contain (first)
      result.nodes must contain(second)
    }

    "Persist and commit graph changes in transaction - add node" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = Await.result(transaction.queryGraph("match (n) return n"), 60 seconds)
      val node = Node.create
      graph.nodes += node
      transaction.commit.persistChanges(graph)

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must contain(node)
    }

    "Persist and commit graph changes in transaction - add relation" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n),(m)")
      val graph = Await.result(transaction.queryGraph("match (n) return n"), 60 seconds)
      val List(first, second) = graph.nodes.toList
      val relation = Relation.create(first, "likes", second)
      graph.relations += relation
      transaction.commit.persistChanges(graph)

      val result = Await.result(db.queryGraph("MATCH (n) OPTIONAL MATCH (n)-[r]-() return n,r"), 60 seconds)
      result.nodes must contain(first)
      result.nodes must contain(second)
      result.relations must contain(relation)
    }

    "Submit last query on commit" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      transaction.commit.queryGraphs("create (y) return y")

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must haveSize(2)
    }

    "only commit with a query" >> {
      val transaction = db.newTransaction()

      transaction.commit.queryGraphs("create (n) return n")

      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must haveSize(1)
    }

    "error on openTransaction is thrown by Transaction" >> {
      val transaction = db.newTransaction()

      transaction.query("boom") must throwA[RuntimeException]

      transaction.isValid mustEqual false
      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must haveSize(0)
    }

    "error on resumeTransaction is thrown by Transaction" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      transaction.query("boom") must throwA[RuntimeException]

      transaction.isValid mustEqual false
      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must haveSize(0)
    }

    "throw error exception on transaction commit" >> {
      db.queryGraph("""create (n:N)-[r:R]->(m)""")

      val transaction = db.newTransaction()
      transaction.query("""MATCH (n:N) DELETE n""") // invalid delete, because (n) has a relation
      transaction.commit() must throwA[RuntimeException]
    }

    "do a manual rollback" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      transaction.rollback()

      transaction.isValid mustEqual false
      val result = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      result.nodes must haveSize(0)
    }

    "be isolated" >> {
      val transaction = db.newTransaction()

      transaction.query("create (n) return n")
      val graph = Await.result(db.queryGraph("match (n) return n"), 60 seconds)
      transaction.rollback()

      graph.nodes must haveSize(0)
    }
  }
}
