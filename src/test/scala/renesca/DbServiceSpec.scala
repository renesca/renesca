package renesca

import org.junit.runner.RunWith
import org.specs2.mock._
import org.specs2.mutable._
import org.specs2.runner.JUnitRunner
import org.specs2.specification.Scope
import renesca.graph.{Graph, Relation, Node}
import renesca.json.{Relationship, PropertyValue, StringPropertyValue}

@RunWith(classOf[JUnitRunner])
class DbServiceSpec extends Specification with Mockito {

  "DbService" can {
    "create an empty graph" in {
      val dbService = new DbService
      dbService.restService = mock[RestService]

      val json = """
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
         } """
      dbService.restService.submit(any[Request]) returns json

      val graph = dbService.queryGraph(mock[Query])

      graph.nodes must beEmpty
      graph.relations must beEmpty
    }


    "create a graph" in {
      val dbService = new DbService
      dbService.restService = mock[RestService]

      val json = """
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
       } """
      dbService.restService.submit(any[Request]) returns json

      val graph = dbService.queryGraph(mock[Query])

      graph.nodes must contain(exactly(Node(1), Node(2)))
      graph.relations must contain(exactly(Relation(9, Node(1), Node(2))))
    }

    "create a graph from multiple results" in {
      val dbService = new DbService
      dbService.restService = mock[RestService]

      val json = """
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
       } """
      dbService.restService.submit(any[Request]) returns json

      val graph = dbService.queryGraph(mock[Query])

      graph.nodes must contain(exactly(Node(1), Node(2), Node(3), Node(4)))
      graph.relations must contain(exactly(
        Relation(9, Node(1), Node(2)),
        Relation(10, Node(3), Node(4)),
        Relation(11, Node(1), Node(3))
      ))
    }

    "create a graph from multiple graph datas" in {
      val dbService = new DbService
      dbService.restService = mock[RestService]

      val json = """
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
       } """
      dbService.restService.submit(any[Request]) returns json

      val graph = dbService.queryGraph(mock[Query])

      graph.nodes must contain(exactly(Node(1), Node(2), Node(3), Node(4)))
      graph.relations must contain(exactly(
        Relation(9, Node(1), Node(2)),
        Relation(10, Node(3), Node(4)),
        Relation(11, Node(1), Node(3))
      ))
    }

    "create a graph from multiple datas - allow data without graph data" in {
      val dbService = new DbService
      dbService.restService = mock[RestService]

      val json = """
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
       } """
      dbService.restService.submit(any[Request]) returns json

      val graph = dbService.queryGraph(mock[Query])

      graph.nodes must contain(exactly(Node(1), Node(2)))
      graph.relations must contain(exactly(
        Relation(9, Node(1), Node(2))
      ))
    }
  }
}

