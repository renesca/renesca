package renesca.json

import spray.json._

object ErrorJsonProtocol extends DefaultJsonProtocol {
  implicit val errorFormat = jsonFormat2(Error)
}


case class Error(code : String, messages : String) 