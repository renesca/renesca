package renesca.json

case class Request(statements:List[Statement])
case class Statement(statement:String, parameters:Map[String, Any], resultDataContents:List[String])
