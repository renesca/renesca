package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import spray.json._

@RunWith(classOf[JUnitRunner])
class RequestSpec extends Specification with Mockito {
  "Request" can {
    import renesca.json.protocols.RequestJsonProtocol._
    "be empty" in {
      val jsonAst =
        """
        {
          "statements" : []
        }
      """.parseJson

      Request().toJson mustEqual jsonAst
    }

    "contain a statement" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN id(n)"
        } ]
      }
                    """.parseJson

      Request(List(Statement("CREATE (n) RETURN id(n)"))).toJson mustEqual jsonAst
    }

    "contain two statements" in {
      val jsonAst = """
      {
        "statements" : [
          {"statement" : "CREATE (n) RETURN id(n)"},
          {"statement" : "CREATE (n) RETURN n"}
         ]
      }
                    """.parseJson

      Request(List(Statement("CREATE (n) RETURN id(n)"), Statement("CREATE (n) RETURN n"))).toJson mustEqual jsonAst
    }

    "contain statement with parameters (string literal)" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.name = { name } RETURN n",
          "parameters" : {"name" : "Glaab"}
        } ]
      }
                    """.parseJson

      Request(List(Statement("MATCH (n) WHERE n.name = { name } RETURN n",
        parameters = Some(Map("name" -> "Glaab"))))).toJson mustEqual jsonAst
    }

    "contain statement with result data contents" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN n",
          "resultDataContents" : [ "row", "graph" ]
        } ]
      }                    """.parseJson

      Request(List(Statement("CREATE (n) RETURN n",
        resultDataContents = Some(List("row", "graph"))))).toJson mustEqual jsonAst
    }
  }

  "Statement" can {
    "contain parameters (long)" >> todo
    "contain parameters (double)" >> todo
    "contain parameters (regular expression)" >> todo
    "contain parameters (properties)" >> todo
    "contain parameters (properties for multiple nodes)" >> todo
    "contain parameters (array of strings)" >> todo
    "contain parameters (array of longs)" >> todo
    "contain parameters (array of doubles)" >> todo
  }

}

