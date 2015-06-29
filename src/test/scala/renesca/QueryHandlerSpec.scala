package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph._
import renesca.json.protocols.ResponseJsonProtocol._
import renesca.parameter.implicits._
import renesca.parameter.{ArrayParameterValue, NullPropertyValue, ParameterValue}
import renesca.table.Table
import spray.json._

@RunWith(classOf[JUnitRunner])
class QueryHandlerSpec extends Specification with Mockito {

  trait GraphQuery extends Scope {
    val dbService = new DbService
    dbService.restService = mock[RestService]

    var graphs: Seq[Graph] = null

    def respond(response: String): Unit = {
      def jsonResponse = response.parseJson.convertTo[json.Response]
      dbService.restService.singleRequest(any[json.Request]) returns jsonResponse
      graphs = dbService.queryGraphs(Query(""))
    }
  }

  //TODO: RestServiceSpec: Throw Exception on authorization Error
  /*
  {
    "errors" : [ {
      "message" : "No authorization header supplied.",
      "code" : "Neo.ClientError.Security.AuthorizationFailed"
    } ]
  }
  */

  "QueryHandler" should {
    "clear changes after persisting raw graph" in {
      val queryHandler = new QueryHandler() {
        override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
        override protected def handleError(exceptions: Option[Exception]) {}
      }

      val graph = mock[Graph]
      graph.changes returns Nil

      queryHandler.persistChanges(graph)

      there was one(graph).clearChanges()
    }

    "clear changes after persisting raw graph on transaction" in {
      val transaction = new Transaction() {
        override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
        override protected def handleError(exceptions: Option[Exception]) {}
      }

      val graph = mock[Graph]
      graph.changes returns Nil

      transaction.persistChanges(graph)

      there was one(graph).clearChanges()
    }

    "clear changes after persisting schema graph" in {
      val queryHandler = new QueryHandler() {
        override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
        override protected def handleError(exceptions: Option[Exception]) {}
      }

      val schemaGraph = new schema.Graph {
        val graph = mock[Graph]
        graph.changes returns Nil
      }

      queryHandler.persistChanges(schemaGraph)

      there was one(schemaGraph.graph).clearChanges()
    }

    "call changes after persisting item" in {
      val queryHandler = new QueryHandler() {
        override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
        override protected def handleError(exceptions: Option[Exception]) {}
      }

      val node = mock[Node]
      node.changes returns Nil

      queryHandler.persistChanges(node)

      there was one(node).changes
    }

    "call changes after persisting schema item" in {
      val queryHandler = new QueryHandler() {
        override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
        override protected def handleError(exceptions: Option[Exception]) {}
      }

      val schemaNode = new schema.Node {
        override val label: Label = Label("hi")
        override val labels: Set[Label] = Set(label)
        val rawItem = mock[Node]
        rawItem.changes returns Nil
      }

      queryHandler.persistChanges(schemaNode)

      there was one(schemaNode.rawItem).changes
    }

    "create no graph data as an empty graph" in new GraphQuery {

      respond( """{"results": [{ "columns": ["n"], "data": [ ] }], "errors": [ ] }""")

      graphs must haveSize(1)
      graphs.head.isEmpty mustEqual true
    }

    "create an empty graph" in new GraphQuery {

      respond( """
          {
            "results": [{ "columns": [ ], "data": [{ "graph": { "nodes": [ ], "relationships": [ ] } }] }],
            "errors": [ ]
          } """)


      graphs must haveSize(1)
      graphs.head.isEmpty mustEqual true
    }


    "create a graph" in new GraphQuery {

      respond( """
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

    "create multiple graphs from multiple results" in new GraphQuery {

      respond( """
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

    "create a graph from multiple graph datas" in new GraphQuery {

      respond( """
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

    "create a graph from multiple datas - allow data without graph data" in new GraphQuery {

      respond( """
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
      def jsonResponse = response.parseJson.convertTo[json.Response]
      dbService.restService.singleRequest(any[json.Request]) returns jsonResponse
      tables = dbService.queryTables(Query(""))
    }
  }

  "create no table data as an empty table" in new TableQuery {

    respond( """{"results": [{ "columns": ["n"], "data": [ ] }], "errors": [ ] }""")

    tables must haveSize(1)
    tables.head.isEmpty mustEqual true
  }

  "create a table" in new TableQuery {
    respond( """
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
        List[ParameterValue](1089, ArrayParameterValue(List(1, 2, 3, 4, 2))),
        List[ParameterValue](1093, NullPropertyValue)
      )
    )
  }

  "create multiple graphs from multiple results" in new TableQuery {
    respond( """
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
        List[ParameterValue](1089, ArrayParameterValue(List(1, 2, 3, 4, 2))),
        List[ParameterValue](1093, NullPropertyValue)
      )
    )
    tables(1) mustEqual Table(
      columns = List("x", "y"),
      data = List(
        List[ParameterValue]("a", 1)
      )
    )
  }

  "create a table from multiple datas - ignore data without row data" in new TableQuery {

    respond( """
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

