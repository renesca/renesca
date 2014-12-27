package renesca.json

import spray.json._ 

object NodeJsonProtocol extends DefaultJsonProtocol {
  implicit val nodeFormat = jsonFormat3(Node)
}

case class Node(id : String, labels : List[String] = Nil, properties : Map[String, JsValue] = Map.empty) 