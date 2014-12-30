package renesca

object RequestType extends Enumeration {
  type RequestType = Value
  val GET,POST = Value
}
import renesca.RequestType._

case class Request(requestType:RequestType, query:String, data:Option[String])

class RestService(host:String) {

  def submit(request:Request):String = ???
}



