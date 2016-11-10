package renesca

package object json {
  import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

  val printer = Printer.noSpaces.copy(dropNullKeys = true)
  def serialize(r: Request) = r.asJson.pretty(printer)

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
}
