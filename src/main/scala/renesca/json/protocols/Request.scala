package renesca.json.protocols

import renesca.json.{ Request,  Statement}
import spray.json._

object StatementJsonProtocol extends DefaultJsonProtocol {
  implicit val statementFormat = jsonFormat3(Statement)
}

object RequestJsonProtocol extends DefaultJsonProtocol {
  import renesca.json.protocols.StatementJsonProtocol._

  implicit val requestFormat = jsonFormat1(Request)
}

