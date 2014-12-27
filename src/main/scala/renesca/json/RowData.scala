package renesca.json

import spray.json._

object RowDataJsonProtocol extends DefaultJsonProtocol {
  implicit val rowDataFormat = jsonFormat0(RowData)
}

case class RowData() 