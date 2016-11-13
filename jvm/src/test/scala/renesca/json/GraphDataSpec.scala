package renesca.json

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

class GraphDataSpec extends Specification with Mockito {
  def decodeGraph(s: String): Graph = decode[Graph](s).toOption.get

  "GraphData" >> {
    "be empty" >> {
      val json = """
        {
          "nodes": [],
          "relationships" : []
        }
                 """
      decodeGraph(json) mustEqual Graph()
    }

    "have nodes" >> {
      val json = """
          {
           "nodes": [{
             "id" : "1",
             "labels" : [],
             "properties" : {}
           }, {
             "id" : "2",
             "labels" : [],
             "properties" : {}
           }],
           "relationships" : []
       }
                        """
      decodeGraph(json) mustEqual Graph(List(Node("1"), Node("2")))
    }

    "have relationships" >> {
      val json = """
           {
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
                        """
      decodeGraph(json) mustEqual Graph(List(Node("1"), Node("2")), List(Relationship("9", "HAS", "1", "2")))
    }
  }
}
