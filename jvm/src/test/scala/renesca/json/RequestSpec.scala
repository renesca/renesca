package renesca.json

import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import renesca._

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import cats.syntax.either._

class RequestSpec extends Specification with Mockito {
  implicit def toJson[T: Encoder](x: T) = x.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)
  def parse(s: String): Json = parser.parse(s).toOption.get
  def reparse(r: Request): Json = parse(serialize(r))

  "Request" >> {
    "be empty" >> {
      val jsonAst = parse(
        """
        {
          "statements" : []
        }
        """
      )

      reparse(Request()) mustEqual jsonAst
    }

    "contain a statement" >> {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN id(n)"
        } ]
      }""")

      reparse(Request(List(Statement("CREATE (n) RETURN id(n)")))) mustEqual jsonAst
    }

    "contain two statements" >> {
      val jsonAst = parse("""
      {
        "statements" : [
          {"statement" : "CREATE (n) RETURN id(n)"},
          {"statement" : "CREATE (n) RETURN n"}
         ]
      }""")

      reparse(Request(List(
        Statement("CREATE (n) RETURN id(n)"),
        Statement("CREATE (n) RETURN n")
      ))) mustEqual jsonAst
    }

    "contain statement with parameters (string literal)" >> {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.name = { name } RETURN n",
          "parameters" : {"name" : "Glaab"}
        } ]
      }""")

      reparse(Request(List(Statement(
        statement = "MATCH (n) WHERE n.name = { name } RETURN n",
        parameters = Some(Map("name" -> "Glaab"))
      )))) mustEqual jsonAst
    }

    "contain statement with result data contents" >> {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN n",
          "resultDataContents" : [ "row", "graph" ]
        } ]
      }""")

      reparse(Request(List(Statement(
        "CREATE (n) RETURN n",
        resultDataContents = Some(List("row", "graph"))
      )))) mustEqual jsonAst
    }
  }

}
