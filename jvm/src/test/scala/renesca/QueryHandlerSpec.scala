package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph._
import renesca.table.Table

import concurrent.{Await, Future}
import concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

@RunWith(classOf[JUnitRunner])
class QueryHandlerSpec extends Specification with Mockito {

  implicit def intToJson(x: Int) = x.asJson
  implicit def stringToJson(x: String) = x.asJson
  implicit def listToJson[T: Encoder](xs: List[T]) = xs.asJson

  trait GraphQuery extends Scope {
    val dbService = new DbService
    dbService.restService = mock[RestService]

    var graphs: Seq[Graph] = null

    def respond(response: String): Unit = {
      def jsonResponse = decode[json.Response](response).toOption.get
      dbService.restService.singleRequest(any[json.Request]) returns Future.successful(jsonResponse)
      graphs = Await.result(dbService.queryGraphs(Query("")), Duration.Inf)
    }
  }

  //TODO: RestServiceSpec: Throw Exception on authorization Error -- isnt this handled by handleErrors already?
  /*
  {
    "errors" : [ {
      "message" : "No authorization header supplied.",
      "code" : "Neo.ClientError.Security.AuthorizationFailed"
    } ]
  }
  */

  "QueryHandler" >> {
    "clear changes after persisting raw graph" >> {
      val queryHandler = new QueryHandler() {
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val graph = mock[Graph]
      graph.changes returns Nil

      queryHandler.persistChanges(graph)

      there was one(graph).clearChanges()
    }

    "pass failure from builder.generateQueries" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Left("meh")
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val graph = mock[Graph]
      graph.changes returns Nil

      val result = queryHandler.persistChanges(graph)

      result mustEqual Some("meh")
      there was no(graph).clearChanges()
    }

    "pass failure from builder.applyQueries" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Right(Seq.empty)
        builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.failed(new Exception("muh"))
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val graph = mock[Graph]
      graph.changes returns Nil

      val result = queryHandler.persistChanges(graph)

      result mustEqual Future.failed(new Exception("muh"))
      there was no(graph).clearChanges()
    }

    "overloaded persistChanges for schema graph" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Right(Seq.empty)
        builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.successful(Unit)
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val schemaGraph = new schema.Graph {
        val graph = mock[Graph]
        graph.changes returns Nil
        def abstractRelations = Seq.empty
        def hyperRelations = Seq.empty
        def nodes = Seq.empty
        def relations = Seq.empty
      }

      queryHandler.persistChanges(schemaGraph)

      there was one(queryHandler.builder).generateQueries(Seq.empty)
      there was one(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
    }

    "fail persistChanges if schema graph cannot be validated" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Right(Seq.empty)
        builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.successful(Unit)
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val schemaNode = new schema.Node {
        override val label: Label = Label("hi")
        override val labels: Set[Label] = Set(label)
        val rawItem = Node(1)
        override def validate = Some("nope")
      }

      val schemaGraph = new schema.Graph {
        val graph = mock[Graph]
        graph.changes returns Seq(SetLabel(schemaNode.rawItem, "meh"))
        def abstractRelations = Seq.empty
        def hyperRelations = Seq.empty
        def nodes = Seq(schemaNode)
        def relations = Seq.empty
      }

      val failure = queryHandler.persistChanges(schemaGraph)

      failure mustEqual Some("Validation for item '(1)' failed: nope")
      there was no(queryHandler.builder).generateQueries(Seq.empty)
      there was no(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
    }

    "overloaded persistChanges for item" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Right(Seq.empty)
        builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.successful(Unit)
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val node = Node(1)
      queryHandler.persistChanges(node)

      there was one(queryHandler.builder).generateQueries(Seq.empty)
      there was one(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
    }

    "overloaded persistChanges for schema item" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Right(Seq.empty)
        builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.successful(Unit)
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val schemaNode = new schema.Node {
        override val label: Label = Label("hi")
        override val labels: Set[Label] = Set(label)
        val rawItem = Node(1)
      }

      queryHandler.persistChanges(schemaNode)

      there was one(queryHandler.builder).generateQueries(Seq.empty)
      there was one(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
    }

    "fail persistChanges if schema item cannot be validated" >> {
      val queryHandler = new QueryHandler() {
        override val builder = mock[QueryBuilder]
        builder.generateQueries(Seq.empty) returns Right(Seq.empty)
        builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Future.successful(Unit)
        override protected def queryService(jsonRequest: json.Request): Future[json.Response] = Future.successful(json.Response())
        override protected def handleError(exceptions: Option[Exception]): Future[Unit] = Future.successful(Unit)
      }

      val schemaNode = new schema.Node {
        override val label: Label = Label("hi")
        override val labels: Set[Label] = Set(label)
        val rawItem = Node(1)
        override def validate = Some("nope")
      }

      val failure = queryHandler.persistChanges(schemaNode)

      failure mustEqual Some("Validation for item '(1)' failed: nope")
      there was no(queryHandler.builder).generateQueries(Seq.empty)
      there was no(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
    }

    "create no graph data as an empty graph" >> new GraphQuery {

      respond("""{"results": [{ "columns": ["n"], "data": [ ] }], "errors": [ ] }""")

      graphs must haveSize(1)
      graphs.head.isEmpty mustEqual true
    }

    "create an empty graph" >> new GraphQuery {

      respond("""
          {
            "results": [{ "columns": [ ], "data": [{ "graph": { "nodes": [ ], "relationships": [ ] } }] }],
            "errors": [ ]
          } """)

      graphs must haveSize(1)
      graphs.head.isEmpty mustEqual true
    }

    "create a graph" >> new GraphQuery {

      respond("""
       {
          "results": [
            {
              "columns": [ ],
              "data": [
                {
                  "graph": {
                    "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
                    "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
                  }
                }
              ]
            }
          ],
          "errors": [ ]
        }""")

      graphs must haveSize(1)
      graphs.head.nodes must contain(exactly(Node(1), Node(2)))
      graphs.head.relations must contain(exactly(Relation(9, Node(1), Node(2), RelationType("HAS"))))
    }

    "create multiple graphs from multiple results" >> new GraphQuery {

      respond("""
       {
          "results": [
            {
              "columns": [ ],
              "data": [
                {
                  "graph": {
                    "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
                    "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
                  }
                }
              ]
            },
            {
              "columns": [ ],
              "data": [
                {
                  "graph": {
                    "nodes": [
                      { "id": "1", "labels": [ ], "properties": { } },
                      { "id": "3", "labels": [ ], "properties": { } },
                      { "id": "4", "labels": [ ], "properties": { } }
                    ],
                    "relationships": [
                      { "id": "10", "type": "HAS", "startNode": "3", "endNode": "4", "properties": { } },
                      { "id": "11", "type": "HAS", "startNode": "1", "endNode": "3", "properties": { } }
                    ]
                  }
                }
              ]
            }
          ],
          "errors": [ ]
        }""")

      graphs must haveSize(2)
      graphs(0).nodes must contain(exactly(Node(1), Node(2)))
      graphs(0).relations must contain(exactly(
        Relation(9, Node(1), Node(2), RelationType("HAS"))
      ))
      graphs(1).nodes must contain(exactly(Node(1), Node(3), Node(4)))
      graphs(1).relations must contain(exactly(
        Relation(10, Node(3), Node(4), RelationType("HAS")),
        Relation(11, Node(1), Node(3), RelationType("HAS"))
      ))
    }

    "create a graph from multiple graph datas" >> new GraphQuery {

      respond("""
       {
          "results": [
            {
              "columns": [ ],
              "data": [
                {
                  "graph": {
                    "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
                    "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
                  }
                },
                {
                  "graph": {
                    "nodes": [
                      { "id": "1", "labels": [ ], "properties": { } },
                      { "id": "3", "labels": [ ], "properties": { } },
                      { "id": "4", "labels": [ ], "properties": { } }
                    ],
                    "relationships": [
                      { "id": "10", "type": "HAS", "startNode": "3", "endNode": "4", "properties": { } },
                      { "id": "11", "type": "HAS", "startNode": "1", "endNode": "3", "properties": { } }
                    ]
                  }
                }
              ]
            }
          ],
          "errors": [ ]
        } """)

      graphs must haveSize(1)
      graphs.head.nodes must contain(exactly(Node(1), Node(2), Node(3), Node(4)))
      graphs.head.relations must contain(exactly(
        Relation(9, Node(1), Node(2), RelationType("HAS")),
        Relation(10, Node(3), Node(4), RelationType("HAS")),
        Relation(11, Node(1), Node(3), RelationType("HAS"))
      ))
    }

    "create a graph from multiple datas - allow data without graph data" >> new GraphQuery {

      respond("""
        {
          "results": [
            {
              "columns": [ ],
              "data": [
                {
                  "graph": {
                    "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
                    "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
                  }
                },
                { }
              ]
            }
          ],
          "errors": [ ]
        }
               """)

      graphs must haveSize(1)
      graphs.head.nodes must contain(exactly(Node(1), Node(2)))
      graphs.head.relations must contain(exactly(
        Relation(9, Node(1), Node(2), RelationType("HAS"))
      ))
    }
  }

  trait TableQuery extends Scope {
    val dbService = new DbService
    dbService.restService = mock[RestService]

    var tables: Seq[Table] = null

    def respond(response: String): Unit = {
      def jsonResponse = decode[json.Response](response).right.get
      dbService.restService.singleRequest(any[json.Request]) returns Future.successful(jsonResponse)
      tables = Await.result(dbService.queryTables(Query("")), Duration.Inf)
    }
  }

  "create no table data as an empty table" >> new TableQuery {

    respond("""{"results": [{ "columns": ["n"], "data": [ ] }], "errors": [ ] }""")

    tables must haveSize(1)
    tables.head.isEmpty mustEqual true
  }

  "create a table" >> new TableQuery {
    respond("""
    {
      "results": [
      {
        "columns": ["id", "n.a"],
        "data": [
        { "row": [620, 1] },
        { "row": [1089, [1, 2, 3, 4, 2]] },
        { "row": [1093, null] }
        ]
      }
      ],
      "errors": [ ]
    }""")

    tables must haveSize(1)
    tables.head mustEqual Table(
      columns = List("id", "n.a"),
      data = List(
        List[ParameterValue](620, 1),
        List[ParameterValue](1089, List(1, 2, 3, 4, 2)),
        List[ParameterValue](1093, Json.Null)
      )
    )
  }

  "create multiple graphs from multiple results" >> new TableQuery {
    respond("""
    {
      "results": [
      {
        "columns": ["id", "n.a"],
        "data": [
        { "row": [620, 1] },
        { "row": [1089, [1, 2, 3, 4, 2]] },
        { "row": [1093, null] }
        ]
      },
      {
        "columns": ["x", "y"],
        "data": [
        { "row": ["a", 1] }
        ]
      }
      ],
      "errors": [ ]
    }""")

    tables must haveSize(2)
    tables(0) mustEqual Table(
      columns = List("id", "n.a"),
      data = List(
        List[ParameterValue](620, 1),
        List[ParameterValue](1089, List(1, 2, 3, 4, 2)),
        List[ParameterValue](1093, Json.Null)
      )
    )
    tables(1) mustEqual Table(
      columns = List("x", "y"),
      data = List(
        List[ParameterValue]("a", 1)
      )
    )
  }

  "create a table from multiple datas - ignore data without row data" >> new TableQuery {

    respond("""
    {
      "results": [
      {
        "columns": ["id", "n.a"],
        "data": [
        { "row": [620, 1] },
        { }
        ]
      }
      ],
      "errors": [ ]
    }""")

    tables must haveSize(1)
    tables(0) mustEqual Table(
      columns = List("id", "n.a"),
      data = List(
        List[ParameterValue](620, 1)
      )
    )
  }
}
