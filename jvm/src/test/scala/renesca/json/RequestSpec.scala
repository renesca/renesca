package renesca.json

import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._
import cats.syntax.either._

@RunWith(classOf[JUnitRunner])
class RequestSpec extends Specification with Mockito {
  implicit def intToJson(x: Int) = x.asJson
  implicit def stringToJson(x: String) = x.asJson
  implicit def listToJson[T: Encoder](xs: List[T]) = xs.asJson
  implicit def keyValue[T: Encoder](t: (String, T)) = (NonBacktickName(t._1), t._2.asJson)

  "Request" can {
    "be empty" in {
      val jsonAst = parse(
        """
        {
          "statements" : []
        }
        """
      ).toOption.get

      Request().asJson mustEqual jsonAst
    }

    "contain a statement" in {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN id(n)"
        } ]
      }""").toOption.get

      Request(List(Statement("CREATE (n) RETURN id(n)"))).asJson mustEqual jsonAst
    }

    "contain two statements" in {
      val jsonAst = parse("""
      {
        "statements" : [
          {"statement" : "CREATE (n) RETURN id(n)"},
          {"statement" : "CREATE (n) RETURN n"}
         ]
      }""").toOption.get

      Request(List(
        Statement("CREATE (n) RETURN id(n)"),
        Statement("CREATE (n) RETURN n")
      )).asJson mustEqual jsonAst
    }

    "contain statement with parameters (string literal)" in {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "MATCH (n) WHERE n.name = { name } RETURN n",
          "parameters" : {"name" : "Glaab"}
        } ]
      }""").toOption.get

      Request(List(Statement(
        statement = "MATCH (n) WHERE n.name = { name } RETURN n",
        parameters = Some(Map("name" -> "Glaab"))
      ))).asJson mustEqual jsonAst
    }

    "contain statement with result data contents" in {
      val jsonAst = parse("""
      {
        "statements" : [ {
          "statement" : "CREATE (n) RETURN n",
          "resultDataContents" : [ "row", "graph" ]
        } ]
      }""").toOption.get

      Request(List(Statement(
        "CREATE (n) RETURN n",
        resultDataContents = Some(List("row", "graph"))
      ))).asJson mustEqual jsonAst
    }
  }

}
