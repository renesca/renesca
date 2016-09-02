package renesca.json

import renesca.Query
import renesca.parameter.ParameterMap

case class Request(statements: List[Statement] = Nil)

object Statement {
  def apply(query: Query, resultDataContents: List[String]): Statement = {
    new Statement(
      query.statement,
      if(query.parameters.nonEmpty) Some(query.parameters) else None,
      if(resultDataContents.nonEmpty) Some(resultDataContents) else None
    )
  }
}

case class Statement(statement: String, parameters: Option[ParameterMap] = None, resultDataContents: Option[List[String]] = None)
