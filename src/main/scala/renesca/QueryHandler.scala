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
import renesca.parameter.{PropertyValue, PropertyKey, ParameterValue, ParameterMap}
import renesca.parameter.implicits._
import renesca.table.Table

import scala.collection.mutable

object Query {
  implicit def stringToQuery(statement: String): Query = Query(statement)
}

case class Query(statement: String, parameters: ParameterMap = Map.empty)

trait QueryInterface {
  def queryWholeGraph: Graph
  def queryGraph(query: Query): Graph
  def queryTable(query: Query): Table
  def queryGraphs(queries: Query*): Seq[Graph]
  def queryTables(queries: Query*): Seq[Table]
  def query(queries: Query*): Unit
  def persistChanges(graph: Graph): Unit
  def persistChanges(schemaGraph: schema.Graph): Unit = persistChanges(schemaGraph.graph)
}

object QueryHandler {
  private def createAndMergeProperties(properties: Properties, uniqueProperties: Seq[PropertyKey]) = {
    val createProperties = properties.filterKeys(!uniqueProperties.contains(_))
    val mergeProperties = properties.filterKeys(uniqueProperties.contains(_))
    val parameterMap = mergeProperties.toMap.map { case (k, v) => (PropertyKey(s"_$k"), v) } ++ Map("createProperties" -> createProperties.toMap)
    val mergePropertiesMatcher = mergeProperties.map { case (k, v) => s"$k: {_$k}" }.mkString(",")
    (mergePropertiesMatcher, parameterMap)
  }

  private def addNodesToQueries(addNodes: Seq[Node]) = {
    addNodes.map( node => {
        val labels = node.labels.map(label => s":`$label`").mkString
        val (queryStr, parameters) = node.properties.unique.map { properties =>
          val (mergePropertiesMatcher, parameterMap) = createAndMergeProperties(node.properties, properties)
          (s"merge (n $labels {$mergePropertiesMatcher}) on create set n += {createProperties} return n", parameterMap)
        }.getOrElse((s"create (n $labels {properties}) return n", Map("properties" -> node.properties.toMap)))

        (Query(queryStr, parameters), (graph: Graph) => {
          val dbNode = graph.nodes.head
          node.properties ++= dbNode.properties
          node.labels ++= dbNode.labels
          node.id.value = dbNode.id.value
        })
    })
  }

  private def addRelationsToQueries(addRelations: Seq[Relation]) = {
   addRelations.map(relation => {
        val (creatorStr, parameters) = relation.properties.unique.map { properties =>
          val (mergePropertiesMatcher, parameterMap) = createAndMergeProperties(relation.properties, properties)
          (s"merge (start)-[r :`${ relation.relationType }` {$mergePropertiesMatcher}]->(end) on create set r += {createProperties} return r", parameterMap)
        }.getOrElse((s"create (start)-[r :`${ relation.relationType }` {properties}]->(end) return r", Map("properties" -> relation.properties.toMap)))

        val queryStr = s"match (start),(end) where id(start) = {startId} and id(end) = {endId} $creatorStr"

        (Query(queryStr, parameters ++ Map("startId" -> relation.startNode.id, "endId" -> relation.endNode.id)), (graph: Graph) => {
          val dbRelation = graph.relations.head
          relation.properties ++= dbRelation.properties
          relation.id.value = dbRelation.id.value
        })
    })
  }

  private def contentChangesToQueries(contentChanges: Seq[GraphContentChange]) = {
    contentChanges.groupBy(_.item).map {
      case (item, changes) =>
        val propertyAdditions: mutable.Map[PropertyKey, ParameterValue] = mutable.Map.empty
        val propertyRemovals: mutable.Set[PropertyKey] = mutable.Set.empty
        val labelAdditions: mutable.Set[Label] = mutable.Set.empty
        val labelRemovals: mutable.Set[Label] = mutable.Set.empty
        val deleteOpt = changes.find {
          case SetProperty(_, key, value) =>
            propertyRemovals -= key
            propertyAdditions += key -> value
            false
          case RemoveProperty(_, key)     =>
            propertyRemovals += key
            propertyAdditions -= key
            false
          case SetLabel(_, label)         =>
            labelRemovals -= label
            labelAdditions += label
            false
          case RemoveLabel(_, label)      =>
            labelRemovals += label
            labelAdditions -= label
            false
          case DeleteItem(_)              =>
            true
        }

        val isRelation = item match {
          case _: Node     => false
          case _: Relation => true
        }

        val variable = "n"
        val matcher = if(isRelation) s"match ()-[$variable]->()" else s"match ($variable)"
        val setters = deleteOpt.map { _ =>
          if(isRelation) {
            s"delete $variable"
          } else {
            val optionalVariable = "m"
            s"optional match ($variable)-[$optionalVariable]-() delete $optionalVariable, $variable"
          }
        }.getOrElse {
          val propertyRemove = propertyRemovals.map(r => s"remove $variable .`$r`").mkString(" ")
          val labelAdd = labelAdditions.map(a => s"set $variable :`$a`").mkString(" ")
          val labelRemove = labelRemovals.map(r => s"remove $variable :`$r`").mkString(" ")
          s"set $variable += {propertyAdditions} $propertyRemove $labelAdd $labelRemove"
        }

        (Query(
          s"$matcher where id($variable) = {itemId} $setters",
          Map("itemId" -> item.id, "propertyAdditions" -> propertyAdditions.toMap)
        ), (graph: Graph) => {})
    }.toSeq
  }

  private def applyQueries(queriesWithCallbacks: Seq[(Query, (Graph) => Unit)], queryHandler: QueryHandler): Unit = {
    val queries = queriesWithCallbacks.map(_._1)
    val callbacks = queriesWithCallbacks.map(_._2)
    queryHandler.queryGraphs(queries: _*).zip(callbacks).foreach { case (g,f) => f(g)}
  }

}

trait QueryHandler extends QueryInterface {

  import renesca.QueryHandler._

  override def queryWholeGraph: Graph = queryGraph("match (n) optional match (n)-[r]-() return n,r")
  override def queryGraph(query: Query): Graph = queryGraphs(query).head
  override def queryTable(query: Query): Table = queryTables(query).head

  override def queryGraphs(queries: Query*): Seq[Graph] = {
    val results = executeQueries(queries, List("graph"))
    extractGraphs(results)
  }

  override def queryTables(queries: Query*): Seq[Table] = {
    val results = executeQueries(queries, List("row"))
    extractTables(results)
  }

  def query(queries: Query*) { executeQueries(queries, Nil) }

  def persistChanges(graph: Graph) {
    val changes = graph.changes

    val contentQueries = contentChangesToQueries(changes.collect { case c: GraphContentChange => c })

    val addNodeQueries = addNodesToQueries(changes.collect { case AddItem(n: Node) => n })
    applyQueries(contentQueries ++ addNodeQueries, this)

    //TODO: merge relation queries into previous request by referring to variables instead of ids
    val addRelationQueries = addRelationsToQueries(changes.collect { case AddItem(r: Relation) => r })
    applyQueries(addRelationQueries, this)

    graph.clearChanges()
  }

  protected def executeQueries(queries: Seq[Query], resultDataContents: List[String]): List[json.Result] = {
    val jsonRequest = buildJsonRequest(queries, resultDataContents)
    val jsonResponse = queryService(jsonRequest)
    handleError(exceptionFromErrors(jsonResponse))
    jsonResponse.results
  }

  protected def extractGraphs(results: Seq[json.Result]): Seq[Graph] = {
    val allJsonGraphs: Seq[List[json.Graph]] = results.map(_.data.flatMap(_.graph))
    allJsonGraphs.map(_.map(Graph(_)).fold(Graph.empty)(_ merge _))
  }

  protected def extractTables(results: Seq[json.Result]): Seq[Table] = {
    results.map(r => Table(r))
  }

  protected def buildJsonRequest(queries: Seq[Query], resultDataContents: List[String]): json.Request = {
    json.Request(queries.map(query => json.Statement(query, resultDataContents)).toList)
  }

  protected def exceptionFromErrors(jsonResponse: json.Response): Option[RuntimeException] = {
    jsonResponse.errors match {
      case Nil    => None
      case errors =>
        val message = errors.map {
          case json.Error(code, msg) => s"$code\n$msg"
        }.mkString("\n", "\n\n", "\n")
        Some(new RuntimeException(message))
    }
  }

  protected def queryService(jsonRequest: json.Request): json.Response
  protected def handleError(exceptions: Option[Exception]): Unit
}

class Transaction extends QueryHandler {thisTransaction =>

  var restService: RestService = null //TODO: inject

  var id: Option[TransactionId] = None

  private var valid = true
  def isValid = valid
  def invalidate() { valid = false }
  private def throwIfNotValid() {
    if(!valid)
      throw new RuntimeException("Transaction is not valid anymore.")
  }


  override protected def queryService(jsonRequest: json.Request): json.Response = {
    throwIfNotValid()
    id match {
      case Some(transactionId) => restService.resumeTransaction(transactionId, jsonRequest)
      case None                =>
        val (transactionId, jsonResponse) = restService.openTransaction(jsonRequest)
        id = Some(transactionId)
        jsonResponse
    }
  }

  protected def handleError(exceptions: Option[Exception]) {
    for(exception <- exceptions) {
      rollback()
      throw exception
    }
  }

  def rollback() = {
    id match {
      case Some(transactionId) => restService.rollbackTransaction(transactionId)
      case None                =>
    }
    invalidate()
  }

  val commit = new QueryHandler {
    // Important:
    // queryService can be called only once, because it commits and invalidates the transaction.
    // This means that methods like persistChanges which are firing multiple queries and thus calling querySerice
    // multiple times need to be modified or wrapped.

    def apply() {
      throwIfNotValid()
      for(transactionId <- id) {
        val jsonResponse = restService.commitTransaction(transactionId)
        handleError(exceptionFromErrors(jsonResponse))
      }

      invalidate()
    }

    override protected def queryService(jsonRequest: json.Request): json.Response = {
      // TODO: share code with this.Transaction.queryService
      throwIfNotValid()
      val jsonResponse = id match {
        case Some(transactionId) => restService.commitTransaction(transactionId, jsonRequest)
        case None                => restService.singleRequest(jsonRequest)
      }

      invalidate()
      jsonResponse
    }

    override def persistChanges(graph: Graph) {
      //TODO: this solution can send one REST request more than needed, as the commit request does not do any changes
      thisTransaction.persistChanges(graph)
      apply() // commit
    }


    override protected def handleError(exceptions: Option[Exception]) = thisTransaction.handleError(exceptions)
  }

  override def toString = s"Transaction(id=$id, valid:$isValid, $restService)"
}

class DbService extends QueryHandler {
  var restService: RestService = null //TODO: inject

  protected def handleError(exceptions: Option[Exception]) {
    for(exception <- exceptions)
      throw exception
  }

  override protected def queryService(jsonRequest: json.Request): json.Response = {
    restService.singleRequest(jsonRequest)
  }

  def newTransaction() = {
    val tx = new Transaction
    tx.restService = restService
    tx
  }

  def transaction(code: Transaction => Any): Unit = {
    val tx = newTransaction
    code(tx)

    // if there was a request and transacion is not done yet
    if(tx.id.isDefined && tx.isValid) tx.commit()
  }
}
