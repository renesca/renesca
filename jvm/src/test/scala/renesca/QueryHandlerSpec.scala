package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph._
import renesca.parameter._

@RunWith(classOf[JUnitRunner])
class QueryHandlerSpec extends Specification with Mockito {

  // trait GraphQuery extends Scope {
  //   val dbService = new DbService
  //   dbService.restService = mock[RestService]

  //   var graphs: Seq[Graph] = null

  //   def respond(response: String): Unit = {
  //     def jsonResponse = response.parseJson.convertTo[json.Response]
  //     dbService.restService.singleRequest(any[json.Request]) returns jsonResponse
  //     graphs = dbService.queryGraphs(Query(""))
  //   }
  // }

  // //TODO: RestServiceSpec: Throw Exception on authorization Error
  // /*
  // {
  //   "errors" : [ {
  //     "message" : "No authorization header supplied.",
  //     "code" : "Neo.ClientError.Security.AuthorizationFailed"
  //   } ]
  // }
  // */

  // "QueryHandler" should {
  //   "clear changes after persisting raw graph" in {
  //     val queryHandler = new QueryHandler() {
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val graph = mock[Graph]
  //     graph.changes returns Nil

  //     queryHandler.persistChanges(graph)

  //     there was one(graph).clearChanges()
  //   }

  //   "pass failure from builder.generateQueries" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Left("meh")
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val graph = mock[Graph]
  //     graph.changes returns Nil

  //     val result = queryHandler.persistChanges(graph)

  //     result mustEqual Some("meh")
  //     there was no(graph).clearChanges()
  //   }

  //   "pass failure from builder.applyQueries" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Right(Seq.empty)
  //       builder.applyQueries(Seq.empty, queryGraphsAndTables) returns Some("muh")
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val graph = mock[Graph]
  //     graph.changes returns Nil

  //     val result = queryHandler.persistChanges(graph)

  //     result mustEqual Some("muh")
  //     there was no(graph).clearChanges()
  //   }

  //   "overloaded persistChanges for schema graph" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Right(Seq.empty)
  //       builder.applyQueries(Seq.empty, queryGraphsAndTables) returns None
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val schemaGraph = new schema.Graph {
  //       val graph = mock[Graph]
  //       graph.changes returns Nil
  //       def abstractRelations = Seq.empty
  //       def hyperRelations = Seq.empty
  //       def nodes = Seq.empty
  //       def relations = Seq.empty
  //     }

  //     queryHandler.persistChanges(schemaGraph)

  //     there was one(queryHandler.builder).generateQueries(Seq.empty)
  //     there was one(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
  //   }

  //   "fail persistChanges if schema graph cannot be validated" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Right(Seq.empty)
  //       builder.applyQueries(Seq.empty, queryGraphsAndTables) returns None
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val schemaNode = new schema.Node {
  //       override val label: Label = Label("hi")
  //       override val labels: Set[Label] = Set(label)
  //       val rawItem = Node(1)
  //       override def validate = Some("nope")
  //     }

  //     val schemaGraph = new schema.Graph {
  //       val graph = mock[Graph]
  //       graph.changes returns Seq(SetLabel(schemaNode.rawItem, "meh"))
  //       def abstractRelations = Seq.empty
  //       def hyperRelations = Seq.empty
  //       def nodes = Seq(schemaNode)
  //       def relations = Seq.empty
  //     }

  //     val failure = queryHandler.persistChanges(schemaGraph)

  //     failure mustEqual Some("Validation for item '(1)' failed: nope")
  //     there was no(queryHandler.builder).generateQueries(Seq.empty)
  //     there was no(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
  //   }

  //   "overloaded persistChanges for item" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Right(Seq.empty)
  //       builder.applyQueries(Seq.empty, queryGraphsAndTables) returns None
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val node = Node(1)
  //     queryHandler.persistChanges(node)

  //     there was one(queryHandler.builder).generateQueries(Seq.empty)
  //     there was one(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
  //   }

  //   "overloaded persistChanges for schema item" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Right(Seq.empty)
  //       builder.applyQueries(Seq.empty, queryGraphsAndTables) returns None
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val schemaNode = new schema.Node {
  //       override val label: Label = Label("hi")
  //       override val labels: Set[Label] = Set(label)
  //       val rawItem = Node(1)
  //     }

  //     queryHandler.persistChanges(schemaNode)

  //     there was one(queryHandler.builder).generateQueries(Seq.empty)
  //     there was one(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
  //   }

  //   "fail persistChanges if schema item cannot be validated" in {
  //     val queryHandler = new QueryHandler() {
  //       override val builder = mock[QueryBuilder]
  //       builder.generateQueries(Seq.empty) returns Right(Seq.empty)
  //       builder.applyQueries(Seq.empty, queryGraphsAndTables) returns None
  //       override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
  //       override protected def handleError(exceptions: Option[Exception]) {}
  //     }

  //     val schemaNode = new schema.Node {
  //       override val label: Label = Label("hi")
  //       override val labels: Set[Label] = Set(label)
  //       val rawItem = Node(1)
  //       override def validate = Some("nope")
  //     }

  //     val failure = queryHandler.persistChanges(schemaNode)

  //     failure mustEqual Some("Validation for item '(1)' failed: nope")
  //     there was no(queryHandler.builder).generateQueries(Seq.empty)
  //     there was no(queryHandler.builder).applyQueries(Seq.empty, queryHandler.queryGraphsAndTables)
  //   }

  //   "create no graph data as an empty graph" in new GraphQuery {

  //     respond( """{"results": [{ "columns": ["n"], "data": [ ] }], "errors": [ ] }""")

  //     graphs must haveSize(1)
  //     graphs.head.isEmpty mustEqual true
  //   }

  //   "create an empty graph" in new GraphQuery {

  //     respond( """
  //         {
  //           "results": [{ "columns": [ ], "data": [{ "graph": { "nodes": [ ], "relationships": [ ] } }] }],
  //           "errors": [ ]
  //         } """)


  //     graphs must haveSize(1)
  //     graphs.head.isEmpty mustEqual true
  //   }


  //   "create a graph" in new GraphQuery {

  //     respond( """
  //      {
  //         "results": [
  //           {
  //             "columns": [ ],
  //             "data": [
  //               {
  //                 "graph": {
  //                   "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
  //                   "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
  //                 }
  //               }
  //             ]
  //           }
  //         ],
  //         "errors": [ ]
  //       }""")

  //     graphs must haveSize(1)
  //     graphs.head.nodes must contain(exactly(Node(1), Node(2)))
  //     graphs.head.relations must contain(exactly(Relation(9, Node(1), Node(2), RelationType("HAS"))))
  //   }

  //   "create multiple graphs from multiple results" in new GraphQuery {

  //     respond( """
  //      {
  //         "results": [
  //           {
  //             "columns": [ ],
  //             "data": [
  //               {
  //                 "graph": {
  //                   "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
  //                   "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
  //                 }
  //               }
  //             ]
  //           },
  //           {
  //             "columns": [ ],
  //             "data": [
  //               {
  //                 "graph": {
  //                   "nodes": [
  //                     { "id": "1", "labels": [ ], "properties": { } },
  //                     { "id": "3", "labels": [ ], "properties": { } },
  //                     { "id": "4", "labels": [ ], "properties": { } }
  //                   ],
  //                   "relationships": [
  //                     { "id": "10", "type": "HAS", "startNode": "3", "endNode": "4", "properties": { } },
  //                     { "id": "11", "type": "HAS", "startNode": "1", "endNode": "3", "properties": { } }
  //                   ]
  //                 }
  //               }
  //             ]
  //           }
  //         ],
  //         "errors": [ ]
  //       }""")

  //     graphs must haveSize(2)
  //     graphs(0).nodes must contain(exactly(Node(1), Node(2)))
  //     graphs(0).relations must contain(exactly(
  //       Relation(9, Node(1), Node(2), RelationType("HAS"))
  //     ))
  //     graphs(1).nodes must contain(exactly(Node(1), Node(3), Node(4)))
  //     graphs(1).relations must contain(exactly(
  //       Relation(10, Node(3), Node(4), RelationType("HAS")),
  //       Relation(11, Node(1), Node(3), RelationType("HAS"))
  //     ))
  //   }

  //   "create a graph from multiple graph datas" in new GraphQuery {

  //     respond( """
  //      {
  //         "results": [
  //           {
  //             "columns": [ ],
  //             "data": [
  //               {
  //                 "graph": {
  //                   "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
  //                   "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
  //                 }
  //               },
  //               {
  //                 "graph": {
  //                   "nodes": [
  //                     { "id": "1", "labels": [ ], "properties": { } },
  //                     { "id": "3", "labels": [ ], "properties": { } },
  //                     { "id": "4", "labels": [ ], "properties": { } }
  //                   ],
  //                   "relationships": [
  //                     { "id": "10", "type": "HAS", "startNode": "3", "endNode": "4", "properties": { } },
  //                     { "id": "11", "type": "HAS", "startNode": "1", "endNode": "3", "properties": { } }
  //                   ]
  //                 }
  //               }
  //             ]
  //           }
  //         ],
  //         "errors": [ ]
  //       } """)

  //     graphs must haveSize(1)
  //     graphs.head.nodes must contain(exactly(Node(1), Node(2), Node(3), Node(4)))
  //     graphs.head.relations must contain(exactly(
  //       Relation(9, Node(1), Node(2), RelationType("HAS")),
  //       Relation(10, Node(3), Node(4), RelationType("HAS")),
  //       Relation(11, Node(1), Node(3), RelationType("HAS"))
  //     ))
  //   }

  //   "create a graph from multiple datas - allow data without graph data" in new GraphQuery {

  //     respond( """
  //       {
  //         "results": [
  //           {
  //             "columns": [ ],
  //             "data": [
  //               {
  //                 "graph": {
  //                   "nodes": [{ "id": "1", "labels": [ ], "properties": { } }, { "id": "2", "labels": [ ], "properties": { } }],
  //                   "relationships": [{ "id": "9", "type": "HAS", "startNode": "1", "endNode": "2", "properties": { } }]
  //                 }
  //               },
  //               { }
  //             ]
  //           }
  //         ],
  //         "errors": [ ]
  //       }
  //              """)

  //     graphs must haveSize(1)
  //     graphs.head.nodes must contain(exactly(Node(1), Node(2)))
  //     graphs.head.relations must contain(exactly(
  //       Relation(9, Node(1), Node(2), RelationType("HAS"))
  //     ))
  //   }
  // }
}

