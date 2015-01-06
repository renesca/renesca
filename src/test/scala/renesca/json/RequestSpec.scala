package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import spray.json._
import PropertyValue._

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
      }""".parseJson

      Request(List(Statement("CREATE (n) RETURN id(n)"))).toJson mustEqual jsonAst
    }

    "contain two statements" in {
      val jsonAst = """
      {
        "statements" : [
          {"statement" : "CREATE (n) RETURN id(n)"},
          {"statement" : "CREATE (n) RETURN n"}
         ]
      }""".parseJson

      Request(List(
        Statement("CREATE (n) RETURN id(n)"),
        Statement("CREATE (n) RETURN n"))
      ).toJson mustEqual jsonAst
    }

    "contain statement with parameters (string literal)" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.name = { name } RETURN n",
          "parameters" : {"name" : "Glaab"}
        } ]
      }""".parseJson

      Request(List(Statement(
        statement = "MATCH (n) WHERE n.name = { name } RETURN n",
        parameters = Some(Map("name" -> "Glaab")))
      )).toJson mustEqual jsonAst
    }

    "contain statement with parameters (double literal)" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.date = { date } RETURN n",
          "parameters" : {"date" : 17.44}
        } ]
      }""".parseJson

      Request(List(Statement(
        statement = "MATCH (n) WHERE n.date = { date } RETURN n",
        parameters = Some(Map("date" -> 17.44)))
      )).toJson mustEqual jsonAst
    }

    "contain statement with parameters (long literal)" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.year = { year } RETURN n",
          "parameters" : {"year" : 1744}
        } ]
      }""".parseJson

      Request(List(Statement(
        statement = "MATCH (n) WHERE n.year = { year } RETURN n",
        parameters = Some(Map("year" -> 1744L)))
      )).toJson mustEqual jsonAst
    }

    "contain statement with parameters (array of strings)" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.param = { strings } RETURN n",
          "parameters" : {"param" : ["1744", "1516"] }
        } ]
      }""".parseJson

      Request(List(Statement(
        statement = "MATCH (n) WHERE n.param = { strings } RETURN n",
        parameters = Some(Map("param" -> ArrayPropertyValue(List(StringPropertyValue("1744"), StringPropertyValue("1516"))))))
      )).toJson mustEqual jsonAst
    }

    "contain statement with result data contents" in {
      val jsonAst = """
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN n",
          "resultDataContents" : [ "row", "graph" ]
        } ]
      }""".parseJson

      Request(List(Statement("CREATE (n) RETURN n",
        resultDataContents = Some(List("row", "graph"))))).toJson mustEqual jsonAst
    }
  }

  "Statement" can {
    "contain parameters (long)" in {
      val statement = Statement("statement", Some(Map("number" -> LongPropertyValue(3L))))
      statement.parameters must not be None
      statement.parameters.get must havePair("number", LongPropertyValue(3))
    }

    "contain parameters (double)" in {
      val statement = Statement("statement", Some(Map("number" -> DoublePropertyValue(3.5))))
      statement.parameters must not be None
      statement.parameters.get must havePair("number", DoublePropertyValue(3.5))
    }

    "contain parameters (regular expression)" >> todo
    "contain parameters (properties)" >> todo
    "contain parameters (properties for multiple nodes)" >> todo

    "contain parameters (array of strings)" in {
      val statement = Statement("statement", Some(Map("strings" -> ArrayPropertyValue(List(StringPropertyValue("a"), StringPropertyValue("b"))))))
      statement.parameters must not be None
      statement.parameters.get must havePair("strings", ArrayPropertyValue(List(StringPropertyValue("a"), StringPropertyValue("b"))))
    }

    "contain parameters (array of longs)" in {
      val statement = Statement("statement", Some(Map("longs" -> ArrayPropertyValue(List(LongPropertyValue(1), LongPropertyValue(2))))))
      statement.parameters must not be None
      statement.parameters.get must havePair("longs", ArrayPropertyValue(List(LongPropertyValue(1), LongPropertyValue(2))))
    }

    "contain parameters (array of doubles)" in {
      val statement = Statement("statement", Some(Map("doubles" -> ArrayPropertyValue(List(DoublePropertyValue(1.5), DoublePropertyValue(2.5))))))
      statement.parameters must not be None
      statement.parameters.get must havePair("doubles", ArrayPropertyValue(List(DoublePropertyValue(1.5), DoublePropertyValue(2.5))))
    }
  }
}
