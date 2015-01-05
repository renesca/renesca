package renesca.json

case class Request(statements:List[Statement] = Nil)
// TODO: parameters can be a lot more than just propertyValues:
// http://neo4j.com/docs/stable/cypher-parameters.html
case class Statement(statement:String, parameters:Option[Map[String,Value]] = None, resultDataContents:Option[List[String]] = None)
