package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

@RunWith(classOf[JUnitRunner])
class GraphDataSpec extends Specification with Mockito {
  def decodeGraph(s: String): Graph = decode[Graph](s).toOption.get

  //TODO: use modern specs syntax: >> instead of can/in/should ...
  //TODO: remove tabs from project
  "GraphData" can {
    "be empty" in {
      val json = """
        {
          "nodes": [],
          "relationships" : []
        }
                 """
      decodeGraph(json) mustEqual Graph()
    }

    "have nodes" in {
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

    "have relationships" in {
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
