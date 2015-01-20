package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph.{Graph, Node, Relation}
import renesca.json.protocols.ResponseJsonProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class DbServiceSpec extends Specification with Mockito {

  trait GraphQuery extends Scope {
    val dbService = new DbService
    dbService.restService = mock[RestService]

    var graph:Graph = null

    def respond(response:String): Unit = {
      def jsonResponse = response.parseJson.convertTo[json.Response]
      dbService.restService.singleRequest(any[json.Request]) returns jsonResponse
      graph = dbService.queryGraph(Query(""))
    }
  }

  "DbService" can {
    "create no graph as an empty graph" in new GraphQuery {

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


      graph.nodes must beEmpty
      graph.relations must beEmpty
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


      graph.nodes must beEmpty
      graph.relations must beEmpty
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

      graph.nodes must contain(exactly(Node(1), Node(2)))
      graph.relations must contain(exactly(Relation(9, Node(1), Node(2))))
    }

    "create a graph from multiple results" in new GraphQuery {

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

      graph.nodes must contain(exactly(Node(1), Node(2), Node(3), Node(4)))
      graph.relations must contain(exactly(
        Relation(9, Node(1), Node(2)),
        Relation(10, Node(3), Node(4)),
        Relation(11, Node(1), Node(3))
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

      graph.nodes must contain(exactly(Node(1), Node(2), Node(3), Node(4)))
      graph.relations must contain(exactly(
        Relation(9, Node(1), Node(2)),
        Relation(10, Node(3), Node(4)),
        Relation(11, Node(1), Node(3))
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

      graph.nodes must contain(exactly(Node(1), Node(2)))
      graph.relations must contain(exactly(
        Relation(9, Node(1), Node(2))
      ))
    }
  }
}

