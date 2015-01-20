package renesca

import akka.actor.ActorSystem
import akka.util.Timeout
import renesca.json.protocols.RequestJsonProtocol._
import renesca.json.protocols.ResponseJsonProtocol._
import spray.client.pipelining._
import spray.http.HttpHeaders.{Location, Accept}
import spray.http.{HttpRequest, _}
import spray.httpx.SprayJsonSupport._
import spray.httpx.unmarshalling._
import MediaTypes._
import HttpCharsets._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

case class TransactionId(id:String) {
  override def toString = id
}

class RestService(server:String) {
  // http://spray.io/documentation/1.2.2/spray-can/http-client/request-level/
  // http://spray.io/documentation/1.2.2/spray-client/
  implicit val system: ActorSystem = ActorSystem()
  implicit val timeout: Timeout = Timeout(15.seconds)
  import system.dispatcher // provides execution context

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  private def awaitResponse(request:HttpRequest):HttpResponse = Await.result(pipeline(request), timeout.duration)

  private def buildHttpRequest(path:String, jsonRequest:json.Request):HttpRequest = {
    //TODO: Accept: application/json; charset=UTF-8 - is this necessary?
    val uri = Uri(s"$server$path")
    val content = jsonRequest
    val accept:MediaRange = `application/json`// withCharset `UTF-8`
    Post(uri, content)//.withHeaders(Accept(accept))
  }

  private def awaitResponse(path:String, jsonRequest:json.Request):(List[HttpHeader], json.Response) = {
    val httpRequest = buildHttpRequest(path, jsonRequest)
    val httpResponse = awaitResponse(httpRequest)
    val Right(jsonResponse) = httpResponse.entity.as[json.Response]
    //TODO: error handling
    (httpResponse.headers, jsonResponse)
  }

  def singleRequest(jsonRequest:json.Request):json.Response = {
    val path = "/db/data/transaction/commit"
    val (_,jsonResponse) = awaitResponse(path, jsonRequest)
    jsonResponse
  }

  def openTransaction(jsonRequest:json.Request = json.Request()):(TransactionId, json.Response) = {
    val path = "/db/data/transaction"
    val (headers, jsonResponse) = awaitResponse(path, jsonRequest)
    val uris = headers.collectFirst({ case Location(uri) => uri })
    val optionId = for (uri <- uris) yield {
      uri.path.reverse.head.toString
    }

    optionId match {
      case Some(id) => (TransactionId(id), jsonResponse)
      case None => throw new RuntimeException("Cannot get transaction id")
    }
  }

  def resumeTransaction(id:TransactionId, jsonRequest:json.Request):json.Response = {
    val path = s"/db/data/transaction/$id"
    val (_,jsonResponse) = awaitResponse(path, jsonRequest)
    jsonResponse
  }

  def commitTransaction(id:TransactionId, jsonRequest:json.Request = json.Request()):json.Response = {
    val path = s"/db/data/transaction/$id/commit"
    val (_,jsonResponse) = awaitResponse(path, jsonRequest)
    jsonResponse
  }
}

