package renesca

import akka.http.scaladsl.marshalling.{Marshal}
import akka.http.scaladsl.model._
import akka.http.scaladsl.model.headers.{Authorization, BasicHttpCredentials, Location, RawHeader}
import akka.util.{ByteString, Timeout}

import scala.collection.mutable
import scala.concurrent.duration._
import scala.concurrent.Future
import akka.http.scaladsl.unmarshalling.{Unmarshal}
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import akka.actor.ActorSystem

import cats.syntax.either._

import io.circe._, io.circe.generic.auto._, io.circe.parser._, io.circe.syntax._

// TODO: type alias / value class
case class TransactionId(id: String) {
  override def toString = id
}
// implicit val actorSystem: ActorSystem = ActorSystem()
// implicit val materializer = ActorMaterializer()

class RestService(
  val server: String,
  credentials: Option[BasicHttpCredentials] = None,
  implicit val actorSystem: ActorSystem,
  implicit val materializer: ActorMaterializer
) {
  // dispatcher provides execution context
  import actorSystem.dispatcher

  private val http = Http()

  private def submitRequest(path: String, request: json.Request): Future[(Seq[HttpHeader], json.Response)] = {
    val headers = new mutable.ListBuffer[HttpHeader]()
    //TODO: Accept: application/json; charset=UTF-8 - is this necessary?
    // val accept:MediaRange = `application/json`// withCharset `UTF-8`
    // headers += Accept(accept)
    headers ++= credentials.map(Authorization(_))
    // accept streaming of results from rest endpoint
    // http://neo4j.com/docs/2.2.3/rest-api-streaming.html
    headers += RawHeader("X-Stream", "true")

    val jsonRequest = request.asJson.noSpaces

    val httpRequest = HttpRequest(
      method = HttpMethods.POST,
      uri = Uri(s"$server$path"),
      headers = headers.toList,
      entity = HttpEntity.Strict(
        MediaTypes.`application/json`,
        ByteString(jsonRequest)
      )
    )

    val httpResponse = http.singleRequest(httpRequest)

    for (
      r <- httpResponse;
      strResponse <- Unmarshal(r.entity).to[String]
    ) yield {
      val response = decode[json.Response](strResponse).valueOr(e => throw e)
      val headers = r.headers
      (headers, response)
    }
  }

  def singleRequest(request: json.Request): Future[json.Response] = {
    val path = "/db/data/transaction/commit"
    for ((_, response) <- submitRequest(path, request)) yield response
  }

  def openTransaction(request: json.Request = json.Request()): Future[(TransactionId, json.Response)] = {
    val path = "/db/data/transaction"
    for ((headers, response) <- submitRequest(path, request)) yield {
      val uris = headers.collectFirst({ case Location(uri) => uri })
      val idOpt = uris.map { _.path.reverse.head.toString } //TODO: something more efficient than .reverse.head?

      idOpt match {
        case Some(id) => (TransactionId(id), response)
        case None => throw new RuntimeException(s"Cannot get transaction id\n$headers\n$response")
      }
    }
  }

  def resumeTransaction(id: TransactionId, request: json.Request): Future[json.Response] = {
    val path = s"/db/data/transaction/$id"
    for ((_, response) <- submitRequest(path, request)) yield response
  }

  def commitTransaction(id: TransactionId, request: json.Request = json.Request()): Future[json.Response] = {
    val path = s"/db/data/transaction/$id/commit"
    for ((_, response) <- submitRequest(path, request)) yield response
  }

  //TODO: Future for error handling?
  def rollbackTransaction(id: TransactionId) {
    val path = s"/db/data/transaction/$id"
    http.singleRequest(HttpRequest(HttpMethods.DELETE, Uri(s"$server$path")))
  }

  override def toString = s"RestService($server${if (credentials.isDefined) " with credentials" else ""})"
}
