package renesca.json

import renesca.Query
import renesca.json.ParameterValue._

case class Request(statements:List[Statement] = Nil)

object Statement {
  def apply(query: Query, resultDataContents: String):Statement = {
    apply(query, List(resultDataContents))
  }

  def apply(query:Query, resultDataContents: List[String]):Statement = {
    new Statement(
      query.statement,
      if (query.parameters.nonEmpty) Some(query.parameters) else None,
      Some(resultDataContents)
    )
  }
}

case class Statement(statement:String, parameters:Option[Map[String,ParameterValue]] = None, resultDataContents:Option[List[String]] = None)
