package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph.{Graph, Node, Relation, RelationType}
import renesca.json.protocols.ResponseJsonProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class QueryHandlerSpec extends Specification with Mockito {

  trait GraphQuery extends Scope {
    val dbService = new DbService
    dbService.restService = mock[RestService]

    var graphs:Seq[Graph] = null

    def respond(response:String): Unit = {
      def jsonResponse = response.parseJson.convertTo[json.Response]
      dbService.restService.singleRequest(any[json.Request]) returns jsonResponse
      graphs = dbService.queryGraphs(Query(""))
    }
  }

  "QueryHandler" should {
    "clear changes after persisting" in {
      val queryHandler = new QueryHandler() {
        override protected def queryService(jsonRequest: json.Request): json.Response = json.Response()
        override protected def handleError(exceptions:Option[Exception]) {}
      }

      val graph = mock[Graph]
      graph.changes returns Nil

      queryHandler.persistChanges(graph)

      there was one(graph).clearChanges()
    }

    "create no graph data as an empty graph" in new GraphQuery {

      respond("""
         {
          "results": [
            {
              "columns": [
                "n"
              ],
              "data": [

              ]
            }
          ],
          "errors": [

          ]
        }
                       """)


      graphs must haveSize(1)
      graphs.head.nodes must beEmpty
      graphs.head.relations must beEmpty
    }

    "create an empty graph" in new GraphQuery {

      respond("""
         {
         "results" : [ {
            "columns" : [],
            "data" : [{
              "graph" : {
                 "nodes": [],
                 "relationships" : []
              }
            }]
          } ],
         "errors" : [ ]
         } """)


      graphs must haveSize(1)
      graphs.head.nodes must beEmpty
      graphs.head.relations must beEmpty
    }


    "create a graph" in new GraphQuery {

      respond("""
       {
       "results" : [ {
          "columns" : [],
          "data" : [{
            "graph" : {
               "nodes": [{
                  "id" : "1",
                  "labels" : [],
                  "properties" : {}
               }, {
                  "id" : "2",
                  "labels" : [],
                  "properties" : {}
               }],
               "relationships" : [{
                   "id":"9",
                   "type":"HAS",
                   "startNode":"1",
                   "endNode":"2",
                   "properties":{}
               }]
            }
          }]
        } ],
       "errors" : [ ]
       } """)

      graphs must haveSize(1)
      graphs.head.nodes must contain(exactly(Node(1), Node(2)))
      graphs.head.relations must contain(exactly(Relation(9, Node(1), Node(2), RelationType("HAS"))))
    }

    "create multiple graphs from multiple results" in new GraphQuery {

      respond("""
       {
       "results" : [ {
          "columns" : [],
          "data" : [{
            "graph" : {
               "nodes": [{
                  "id" : "1",
                  "labels" : [],
                  "properties" : {}
               }, {
                  "id" : "2",
                  "labels" : [],
                  "properties" : {}
               }],
               "relationships" : [{
                   "id":"9",
                   "type":"HAS",
                   "startNode":"1",
                   "endNode":"2",
                   "properties":{}
               }]
            }
          }]
         }, {
           "columns" : [],
           "data" : [{
             "graph" : {
                "nodes": [{
                   "id" : "1",
                   "labels" : [],
                   "properties" : {}
                }, {
                   "id" : "3",
                   "labels" : [],
                   "properties" : {}
                }, {
                   "id" : "4",
                   "labels" : [],
                   "properties" : {}
                }],
                "relationships" : [{
                    "id":"10",
                    "type":"HAS",
                    "startNode":"3",
                    "endNode":"4",
                    "properties":{}
                }, {
                   "id":"11",
                   "type":"HAS",
                   "startNode":"1",
                   "endNode":"3",
                   "properties":{}
               }]
             }
           }]
         } ],
       "errors" : [ ]
       } """)

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

      respond("""
       {
       "results" : [ {
          "columns" : [],
          "data" : [{
            "graph" : {
               "nodes": [{
                  "id" : "1",
                  "labels" : [],
                  "properties" : {}
               }, {
                  "id" : "2",
                  "labels" : [],
                  "properties" : {}
               }],
               "relationships" : [{
                   "id":"9",
                   "type":"HAS",
                   "startNode":"1",
                   "endNode":"2",
                   "properties":{}
               }]
            }
          }, {
            "graph" : {
               "nodes": [{
                  "id" : "1",
                  "labels" : [],
                  "properties" : {}
               }, {
                  "id" : "3",
                  "labels" : [],
                  "properties" : {}
               }, {
                  "id" : "4",
                  "labels" : [],
                  "properties" : {}
               }],
               "relationships" : [{
                   "id":"10",
                   "type":"HAS",
                   "startNode":"3",
                   "endNode":"4",
                   "properties":{}
               }, {
                  "id":"11",
                  "type":"HAS",
                  "startNode":"1",
                  "endNode":"3",
                  "properties":{}
              }]
            }
          }]
        } ],
       "errors" : [ ]
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

      respond("""
       {
       "results" : [ {
          "columns" : [],
          "data" : [{
            "graph" : {
               "nodes": [{
                  "id" : "1",
                  "labels" : [],
                  "properties" : {}
               }, {
                  "id" : "2",
                  "labels" : [],
                  "properties" : {}
               }],
               "relationships" : [{
                   "id":"9",
                   "type":"HAS",
                   "startNode":"1",
                   "endNode":"2",
                   "properties":{}
               }]
            }
          }, {

          }]
        } ],
       "errors" : [ ]
       } """)

      graphs must haveSize(1)
      graphs.head.nodes must contain(exactly(Node(1), Node(2)))
      graphs.head.relations must contain(exactly(
        Relation(9, Node(1), Node(2), RelationType("HAS"))
      ))
    }
  }
}

