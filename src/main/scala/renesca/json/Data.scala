package renesca.json

import spray.json._
import RowDataJsonProtocol._
import GraphDataJsonProtocol._

object DataJsonProtocol extends DefaultJsonProtocol {
  implicit val dataFormat = jsonFormat2(Data)
}

case class Data(row : Option[RowData] = None, graph : Option[GraphData] = None) 