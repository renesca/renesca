package renesca.json
// TODO: move out of json package

import io.circe.Json
import renesca.ParameterMap

case class Request(
  statements: List[Statement] = Nil
)

case class Statement(
  statement: String,
  parameters: Option[ParameterMap] = None,
  resultDataContents: Option[List[String]] = None
)

object Statement {
  import renesca.Query

  def apply(query: Query, resultDataContents: List[String]): Statement = {
    new Statement(
      query.statement,
      if (query.parameters.nonEmpty) Some(query.parameters) else None,
      if (resultDataContents.nonEmpty) Some(resultDataContents) else None
    )
  }
}
