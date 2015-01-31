package renesca.json.protocols

import renesca.json.protocols.ParameterProtocol._
import renesca.json.{Request, Statement}
import spray.json._

object RequestJsonProtocol extends DefaultJsonProtocol {
  implicit val statementFormat = jsonFormat3(Statement.apply)
  implicit val requestFormat = jsonFormat1(Request)
}

