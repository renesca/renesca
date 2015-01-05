package renesca.json.protocols

import renesca.json.{Request, Statement}
import spray.json._

import ValueProtocol._

object RequestJsonProtocol extends DefaultJsonProtocol {
  implicit val statementFormat = jsonFormat3(Statement)
  implicit val requestFormat = jsonFormat1(Request)
}

