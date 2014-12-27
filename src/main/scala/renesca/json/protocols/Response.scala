package renesca.json.protocols

import renesca.json._
import spray.json.DefaultJsonProtocol


object RelationshipJsonProtocol extends DefaultJsonProtocol {
  implicit val relationshipFormat = jsonFormat5(Relationship)
}

object NodeJsonProtocol extends DefaultJsonProtocol {
  implicit val nodeFormat = jsonFormat3(Node)
}

object GraphDataJsonProtocol extends DefaultJsonProtocol {
  import renesca.json.protocols.NodeJsonProtocol._
  import renesca.json.protocols.RelationshipJsonProtocol._

  implicit val graphDataFormat = jsonFormat2(GraphData)
}

object ErrorJsonProtocol extends DefaultJsonProtocol {
  implicit val errorFormat = jsonFormat2(Error)
}

object RowDataJsonProtocol extends DefaultJsonProtocol {
  implicit val rowDataFormat = jsonFormat0(RowData)
}

object DataJsonProtocol extends DefaultJsonProtocol {
  import renesca.json.protocols.GraphDataJsonProtocol._
  import renesca.json.protocols.RowDataJsonProtocol._

  implicit val dataFormat = jsonFormat2(Data)
}

object ResultJsonProtocol extends DefaultJsonProtocol {
  import renesca.json.protocols.DataJsonProtocol._

  implicit val resultFormat = jsonFormat2(Result)
}


object ResponseJsonProtocol extends DefaultJsonProtocol {
  import renesca.json.protocols.ErrorJsonProtocol._
  import renesca.json.protocols.ResultJsonProtocol._

  implicit val responseFormat = jsonFormat2(Response)
}
