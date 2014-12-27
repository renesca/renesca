package renesca.json

import spray.json._ 

object RelationshipJsonProtocol extends DefaultJsonProtocol {
  implicit val relationshipFormat = jsonFormat5(Relationship)
}

case class Relationship(
          id : String,
          `type` : String,
          startNode:String,
          endNode:String,
          properties : Map[String, JsValue] = Map.empty
          ) 