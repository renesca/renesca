package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca._

import io.circe._, io.circe.generic.auto._, io.circe.syntax._
import cats.syntax.either._

@RunWith(classOf[JUnitRunner])
class RequestSpec extends Specification with Mockito {
  implicit def toJson[T: Encoder](x: T) = x.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)
  def parse(s: String): Json = parser.parse(s).toOption.get
  def reparse(r: Request): Json = parse(serialize(r))

  "Request" can {
    "be empty" in {
      val jsonAst = parse(
        """
        {
          "statements" : []
        }
        """
      )

      reparse(Request()) mustEqual jsonAst
    }

    "contain a statement" in {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN id(n)"
        } ]
      }""")

      reparse(Request(List(Statement("CREATE (n) RETURN id(n)")))) mustEqual jsonAst
    }

    "contain two statements" in {
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

    "contain statement with parameters (string literal)" in {
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

    "contain statement with result data contents" in {
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
