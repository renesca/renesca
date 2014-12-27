package renesca.json

import spray.json._
import spray.json.DefaultJsonProtocol
import ResultJsonProtocol._
import ErrorJsonProtocol._

object ResponseJsonProtocol extends DefaultJsonProtocol {
  implicit val responseFormat = jsonFormat2(Response)
}

case class Response(results : List[Result] = Nil, errors : List[Error] = Nil) 