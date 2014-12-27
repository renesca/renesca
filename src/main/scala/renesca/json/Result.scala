package renesca.json

import spray.json._
import DataJsonProtocol._

object ResultJsonProtocol extends DefaultJsonProtocol {
  implicit val resultFormat = jsonFormat2(Result)
}

case class Result(columns : List[String], data : List[Data]) 