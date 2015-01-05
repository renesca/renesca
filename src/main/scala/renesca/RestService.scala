package renesca

import akka.actor.ActorSystem
import akka.util.Timeout
import spray.client.pipelining._
import spray.http.{HttpRequest, _}
import spray.httpx.SprayJsonSupport._

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

  def buildHttpRequest(jsonRequest:json.Request) = {
    import renesca.json.protocols.RequestJsonProtocol._

    Post("http://localhost:7474/db", jsonRequest)
  }

  def awaitJsonResponse(jsonRequest:json.Request):json.Response = {
    val httpRequest = buildHttpRequest(jsonRequest)
    val httpResponse = awaitResponse(httpRequest)
    json.Response()
  }
}

