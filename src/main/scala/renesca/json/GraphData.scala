package renesca.json

import spray.json._
import NodeJsonProtocol._
import RelationshipJsonProtocol._

object GraphDataJsonProtocol extends DefaultJsonProtocol {
  implicit val graphDataFormat = jsonFormat2(GraphData)
}

case class GraphData(nodes : List[Node] = Nil, relationships : List[Relationship] = Nil) 