package renesca.json.protocols

import renesca.json._
import spray.json.DefaultJsonProtocol

object ResponseJsonProtocol extends DefaultJsonProtocol {

  import renesca.json.protocols.ParameterProtocol._

  implicit val relationshipFormat = jsonFormat5(Relationship)
  implicit val nodeFormat = jsonFormat3(Node)
  implicit val graphDataFormat = jsonFormat2(Graph)
  implicit val errorFormat = jsonFormat2(Error)
  implicit val dataFormat = jsonFormat2(Data)
  implicit val resultFormat = jsonFormat2(Result)
  implicit val transactionFormat = jsonFormat1(Transaction)
  implicit val responseFormat = jsonFormat4(Response)
}
