package renesca

import akka.actor.ActorSystem
import akka.util.Timeout
import renesca.json.protocols.RequestJsonProtocol._
import renesca.json.protocols.ResponseJsonProtocol._
import spray.client.pipelining._
import spray.http.{HttpRequest, _}
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class RestService {
  // http://spray.io/documentation/1.2.2/spray-can/http-client/request-level/
  // http://spray.io/documentation/1.2.2/spray-client/
  implicit val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = Timeout(15.seconds)
  import system.dispatcher // provides execution context

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
  def awaitResponse(request:HttpRequest):HttpResponse = Await.result(pipeline(request), timeout.duration)

  def buildHttpRequest(jsonRequest:json.Request):HttpRequest = {
    //TODO: Accept: application/json; charset=UTF-8
    //TODO: don't hard-code URI
    Post("http://localhost:7474/db/data/transaction/commit", jsonRequest)
  }

  def awaitJsonResponse(jsonRequest:json.Request):json.Response = {
    val httpRequest = buildHttpRequest(jsonRequest)
    val httpResponse = awaitResponse(httpRequest)
    val Right(jsonResponse) = httpResponse.entity.as[json.Response]
    //TODO: error handling
    jsonResponse
  }
}

