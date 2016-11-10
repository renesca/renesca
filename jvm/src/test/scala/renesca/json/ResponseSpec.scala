package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

@RunWith(classOf[JUnitRunner])
class ResponseSpec extends Specification with Mockito {
  implicit def intToJson(x: Int) = x.asJson
  implicit def stringToJson(x: String) = x.asJson
  implicit def listToJson[T: Encoder](xs: List[T]) = xs.asJson
  implicit def mapToJson[T: Encoder](xs: Map[String, T]) = xs.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  "Response" >> {
    "be empty" >> {
      val json = """
        {
          "results" : [ ],
          "errors" : [ ]
        }
                 """
      val response = decode[Response](json).toOption.get

      response mustEqual Response()
    }

    "contain one result" >> {
      val json = """
        {
          "results" : [ {
            "columns" : ["col1", "col2"],
            "data" : []
          } ],
          "errors" : [ ]
        }
                 """
      val response = decode[Response](json).toOption.get
      response mustEqual Response(results = List(Result(List("col1", "col2"), Nil)))
    }

    "contain data" >> {
      val json = """
           {
           "results" : [ {
              "columns" : ["col1"],
              "data" : [{}]
            } ],
           "errors" : [ ]
           }
                        """
      val response = decode[Response](json).toOption.get
      response mustEqual Response(results = List(Result(List("col1"), List(Data()))))
    }

    "contain error" >> {
      val json = """
          {
            "results" : [ ],
            "errors" : [ {
              "code" : "Neo.ClientError.Statement.InvalidSyntax",
              "message" : "Invalid input 'T': expected <init> (line 1, column 1)\n\"This is not a valid Cypher Statement.\"\n ^"
            } ]
          }                        """
      val response = decode[Response](json).toOption.get
      response mustEqual Response(errors = List(
        Error(
          code = "Neo.ClientError.Statement.InvalidSyntax",
          message = "Invalid input 'T': expected <init> (line 1, column 1)\n\"This is not a valid Cypher Statement.\"\n ^"
        )
      ))
    }

    "contain transaction information" >> {
      val json = """
          {
            "commit": "http:\/\/localhost:7474\/db\/data\/transaction\/29\/commit",
            "results" : [ ],
            "transaction": {
              "expires": "Tue, 27 Jan 2015 17:37:30 +0000"
            },
            "errors": [

            ]
          }                        """
      val response = decode[Response](json).toOption.get
      response mustEqual Response(
        commit = Some("http://localhost:7474/db/data/transaction/29/commit"),
        transaction = Some(Transaction("Tue, 27 Jan 2015 17:37:30 +0000"))
      )
    }

    "handle null" >> {
      val json = """
          {
            "results": [
              {
                "columns": ["nulled", "nulledarray"],
                "data": [
                  {
                    "graph": { "nodes": [ ], "relationships": [ ] },
                    "row": [null, [null, null]]
                  }
                ]
              }
            ],
            "errors": [ ]
          }
                 """

      val response = decode[Response](json).toOption.get
      response mustEqual Response(results = List(Result(List("nulled", "nulledarray"), List(Data(
        row = Some(List(Json.Null, List(Json.Null, Json.Null))),
        graph = Some(Graph())
      )))))
    }

    "parse complicated result" >> {
      val json = """
          {
            "results" : [ {
              "columns" : [ "bike", "p1", "p2" ],
              "data" : [ {
                "row" : [ {
                  "weight" : 10
                }, [ {
                  "weight" : 10
                }, {
                  "position" : 1
                }, {
                  "spokes" : 3
                } ], [ {
                  "weight" : 10
                }, {
                  "position" : 2
                }, {
                  "spokes" : 32
                } ] ],
                "graph" : {
                  "nodes" : [ {
                    "id" : "4",
                    "labels" : [ "Bike" ],
                    "properties" : {
                      "weight" : 10
                    }
                  }, {
                    "id" : "5",
                    "labels" : [ "Wheel" ],
                    "properties" : {
                      "spokes" : 3
                    }
                  }, {
                    "id" : "6",
                    "labels" : [ "Wheel" ],
                    "properties" : {
                      "spokes" : 32
                    }
                  } ],
                  "relationships" : [ {
                    "id" : "0",
                    "type" : "HAS",
                    "startNode" : "4",
                    "endNode" : "5",
                    "properties" : {
                      "position" : 1
                    }
                  }, {
                    "id" : "1",
                    "type" : "HAS",
                    "startNode" : "4",
                    "endNode" : "6",
                    "properties" : {
                      "position" : 2
                    }
                  } ]
                }
              } ]
            } ],
            "errors" : [ ]
          }             """
      val response = decode[Response](json).toOption.get
      response mustEqual Response(None, List(Result(
        List("bike", "p1", "p2"),
        List(Data(
          row = Some(List(
            Map("weight" -> 10),
            List(
              Map("weight" -> 10),
              Map("position" -> 1),
              Map("spokes" -> 3)
            ),
            (List(
              Map("weight" -> 10),
              Map("position" -> 2),
              Map("spokes" -> 32)
            ))
          )),
          graph = Some(Graph(
            List(
              Node("4", List("Bike"), Map("weight" -> 10)),
              Node("5", List("Wheel"), Map("spokes" -> 3)),
              Node("6", List("Wheel"), Map("spokes" -> 32))
            ),
            List(
              Relationship("0", "HAS", "4", "5", Map("position" -> 1)),
              Relationship("1", "HAS", "4", "6", Map("position" -> 2))
            )
          ))
        ))
      )),
        None, List())
    }

  }
}
