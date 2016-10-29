package renesca

// QueryHandler ensures that we have the same query-interface in Transaction and in DbService.
// Both must implement queryService and handleError.
// The query-interface consists of the methods:
//  queryGraph  - submit one query [with parameters] and return a Graph
//  queryTable  - submit one query [with parameters] and return a row set
//  queryGraphs - submit multiple queries [with parameters] and return a Graph for each result
//  queryTables - submit multiple queries [with parameters] and return a row set for each result
//  query       - submit one or multiple queries [with parameters] as a side effect (returns Unit)
//  persist     - save a modified graph to the database

import renesca.graph._
import renesca.parameter._
import renesca.schema.{AbstractRelation, HyperRelation}

import org.neo4j.driver.v1.{types => neo4j}
import org.neo4j.driver.v1._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

object Query {
  implicit def stringToQuery(statement: String): Query = Query(statement)
}

case class Query(statement: String, parameters: ParameterMap = Map.empty)

object SchemaValidator {
  def validateSchemaGraph(schemaGraph: schema.Graph): Option[String] = {
    val changes = schemaGraph.graph.changes.collect { case c: GraphItemChange => c.item }.toSet
    val items = (schemaGraph.nodes ++ schemaGraph.relations).filter(changes contains _.rawItem)
    validateSchemaItems(items)
  }

  def validateSchemaItems(items: Iterable[schema.Item]): Option[String] = {
    val validations = items.map { item =>
      item.validate match {
        case Some(err) => Some(s"Validation for item '${item.rawItem}' failed: $err")
        case None      => None
      }
    }.flatten

    if (validations.isEmpty)
      None
    else
      Some(validations.mkString(","))
  }
}

trait QueryInterface {
  import SchemaValidator._

  def queryGraph(query: Query)(implicit runner: StatementRunner): Future[Graph]
  def queryRecords(query: Query)(implicit runner: StatementRunner): Future[Seq[Record]]
  def query(queries: Query)(implicit runner: StatementRunner): Future[summary.ResultSummary]

  def queryGraph(statement: String, parameters: ParameterMap = Map.empty)(implicit runner: StatementRunner): Future[Graph] = queryGraph(Query(statement, parameters))
  def queryRecord(statement: String, parameters: ParameterMap = Map.empty)(implicit runner: StatementRunner): Future[Seq[Record]] = queryRecords(Query(statement, parameters))
  def query(statement: String, parameters: ParameterMap = Map.empty)(implicit runner: StatementRunner): Unit = query(Query(statement, parameters))

  def persistChanges(graph: Graph)(implicit runner: Transaction): Option[String]

  def persistChanges(schemaGraph: schema.Graph)(implicit runner: Transaction): Option[String] = {
    validateSchemaGraph(schemaGraph) match {
      case Some(err) => Some(err)
      case None      => persistChanges(schemaGraph.graph)
    }
  }

  def persistChanges(item: Item, items: Item*)(implicit runner: Transaction): Option[String] = {
    val allItems = item :: items.toList
    val graph = Graph(allItems.collect { case n: Node => n }, allItems.collect { case r: Relation => r })
    persistChanges(graph)
  }

  def persistChanges(item: schema.Item, items: schema.Item*)(implicit runner: Transaction): Option[String] = {
    val allItems = item :: items.toList
    validateSchemaItems(allItems) match {
      case Some(err) => Some(err)
      case None =>
        val schemaGraph = new schema.Graph {
          // the schema graph methods will never be called,
          // but we use the implementation of the add function,
          // which also adds the path of hyperrelations to the graph.
          override def nodes: Seq[_ <: schema.Node] = ???
          override def hyperRelations: Seq[_ <: HyperRelation[_, _, _, _, _]] = ???
          override def relations: Seq[_ <: schema.Relation[_, _]] = ???
          override def abstractRelations: Seq[_ <: AbstractRelation[_, _]] = ???
          val graph: Graph = Graph.empty
        }

        schemaGraph.add(allItems: _*)
        persistChanges(schemaGraph.graph)
    }
  }
}

class QueryHandler extends QueryInterface {
  val builder = new QueryBuilder

  override def queryGraph(query: Query)(implicit runner: StatementRunner): Future[Graph] = {
    queryRecords(query) map Neo4jTranslation.recordsToGraph
  }

  override def queryRecords(query: Query)(implicit runner: StatementRunner): Future[Seq[Record]] = Future {
    import scala.collection.JavaConversions._
    runner.run(query.statement, query.parameters.map { case (k,v) => k.name -> v }).toList
  }

  def query(query: Query)(implicit runner: StatementRunner): Future[summary.ResultSummary] = Future {
    import scala.collection.JavaConversions._
    runner.run(query.statement, query.parameters.map { case (k,v) => k.name -> v }).consume()
  }

  def persistChanges(graph: Graph)(implicit runner: Transaction): Option[String] = {
    builder.generateQueries(graph.changes) match {
      case Left(msg) => Some(msg)
      case Right(queries) =>
        val failure = builder.applyQueries(queries, queryRecords)
        if (failure.isEmpty)
          graph.clearChanges()

        failure
    }
  }
}
