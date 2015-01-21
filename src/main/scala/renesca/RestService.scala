package renesca

import akka.actor.ActorSystem
import akka.util.Timeout
import renesca.json.protocols.RequestJsonProtocol._
import renesca.json.protocols.ResponseJsonProtocol._
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.{HttpRequest, _}
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import MediaTypes._
import HttpCharsets._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class RestService(server:String) {
  // http://spray.io/documentation/1.2.2/spray-can/http-client/request-level/
  // http://spray.io/documentation/1.2.2/spray-client/
  implicit val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = Timeout(15.seconds)
  import system.dispatcher // provides execution context

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
  def awaitResponse(request:HttpRequest):HttpResponse = Await.result(pipeline(request), timeout.duration)

  def buildHttpRequest(jsonRequest:json.Request):HttpRequest = {
    //TODO: Accept: application/json; charset=UTF-8 - is this necessary?
    val path = "/db/data/transaction/commit"
    val uri = Uri(s"$server$path")
    val content = jsonRequest
    val accept:MediaRange = `application/json`// withCharset `UTF-8`
    Post(uri, content)//.withHeaders(Accept(accept))
  }

  def awaitJsonResponse(jsonRequest:json.Request):json.Response = {
    val httpRequest = buildHttpRequest(jsonRequest)
    val httpResponse = awaitResponse(httpRequest)
    httpResponse.entity.as[json.Response] match {
      case Right(jsonResponse) =>
        jsonResponse
      case Left(error) =>
        println(httpResponse.status)
        println(httpResponse.entity.asString)
        println(error)
        null
    }
  }
}

