package renesca

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import renesca.json.PropertyKey._

@RunWith(classOf[JUnitRunner])
class TransactionSpec extends IntegrationSpecification {
  "Transactions" should {
    "BatchQuery rollback on errors" in {
      try {
        db.batchQuery(List(Query("create (n)"), Query("blabla illegal query")))
      } catch {
        case e:RuntimeException =>
      }

      val graph = db.queryGraph(Query("match n return n"))
      graph.isEmpty mustEqual true
    }
    "Manual transaction rollback on errors" in {
      val transaction = new Transaction
      transaction.restService = db.restService // TODO injection
      try {
        transaction.batchQuery(List(Query("create (n)"), Query("blabla illegal query")))
      } catch {
        case e:RuntimeException =>
      }

      transaction.commit()
      val graph = db.queryGraph(Query("match n return n"))
      graph.isEmpty mustEqual true
    }
    "Manual transaction success" in {
      val transaction = new Transaction
      transaction.restService = db.restService // TODO injection

      transaction.batchQuery("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      transaction.batchQuery("match (n) where id(n) = {id} delete n", Map("id" -> first.id))
      transaction.commit()

      val result = db.queryGraph(Query("match n return n"))
      result.nodes must not contain (first)
      result.nodes must contain (second)
    }
    "Persist graph changes in transaction" in {
      val transaction = new Transaction
      transaction.restService = db.restService // TODO injection

      transaction.batchQuery("create (n),(m)")
      val graph = transaction.queryGraph("match (n) return n")
      val List(first, second) = graph.nodes.toList
      graph.delete(first)
      transaction.persistChanges(graph)
      transaction.commit()

      val result = db.queryGraph(Query("match n return n"))
      result.nodes must not contain (first)
      result.nodes must contain (second)
    }
  }
}
