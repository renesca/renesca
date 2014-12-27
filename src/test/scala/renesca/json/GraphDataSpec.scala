package renesca.json

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import renesca.json.protocols.GraphDataJsonProtocol
import spray.json._
import GraphDataJsonProtocol._

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
       val graphData = json.parseJson.convertTo[GraphData]
       graphData mustEqual GraphData()
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
			 val graphData = json.parseJson.convertTo[GraphData]
			 graphData mustEqual GraphData(List(Node("1"), Node("2")))
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
    	 val graphData = json.parseJson.convertTo[GraphData]
			 graphData mustEqual GraphData(List(Node("1"), Node("2")), List(Relationship("9","HAS","1","2")))
     }
     
     "nodes have properties" in {
       todo
     }
  }
}
