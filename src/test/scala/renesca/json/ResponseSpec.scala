package renesca.json

import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import org.specs2.mutable.Specification
import org.specs2.mock.Mockito
import spray.json._
import ResponseJsonProtocol._

@RunWith(classOf[JUnitRunner])
class ResponseSpec extends Specification with Mockito {
  "Response" can {
     "be empty" in {
       val json = """
        {
          "results" : [ ],
          "errors" : [ ]
        }
        """
       val response = json.parseJson.convertTo[Response]

       response mustEqual Response()
     } 

     "contain one result" in {
       val json = """
        {
          "results" : [ { 
            "columns" : ["col1", "col2"], 
            "data" : []
          } ],
          "errors" : [ ]
        }
        """
       val response = json.parseJson.convertTo[Response]
       response mustEqual Response(List(Result(List("col1", "col2"), Nil)))
     } 

     "contain data" in {
    	 val json = """
    			 {
    			 "results" : [ { 
              "columns" : ["col1"],
              "data" : [{}]
            } ],
    			 "errors" : [ ]
    			 }
    			 """
    			 val response = json.parseJson.convertTo[Response]
    			 response mustEqual Response(List(Result(List("col1"), List(Data()))))
     } 
  }
}