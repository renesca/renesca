package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.json.protocols.ResponseJsonProtocol._
import spray.json._

@RunWith(classOf[JUnitRunner])
class GraphDataSpec extends Specification with Mockito {
  "GraphData" can {
    "be empty" in {
      val json = """
        {
          "nodes": [],
          "relationships" : []
        }
                 """
      val graphData = json.parseJson.convertTo[Graph]
      graphData mustEqual Graph()
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
      val graphData = json.parseJson.convertTo[Graph]
      graphData mustEqual Graph(List(Node("1"), Node("2")))
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
      val graphData = json.parseJson.convertTo[Graph]
      graphData mustEqual Graph(List(Node("1"), Node("2")), List(Relationship("9", "HAS", "1", "2")))
    }

    "nodes have properties" in {
      todo
    }
  }
}
