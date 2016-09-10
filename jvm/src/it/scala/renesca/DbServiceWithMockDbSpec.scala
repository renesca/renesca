package renesca

import com.github.httpmock.api.Stubbing
import com.github.httpmock.builder.RequestBuilder._
import com.github.httpmock.builder.ResponseBuilder._
import com.github.httpmock.dto.ResponseDto
import com.github.httpmock.specs.{HttpMock, HttpMockServer}
import org.junit.runner.RunWith
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner
import renesca.graph.Graph
import spray.json._


@RunWith(classOf[JUnitRunner])
class DbServiceWithMockDbSpec extends HttpMockSpecification {
  "DbService" should {
    "execute graph query" in new DbMock(this) {
      val A = json.Node("1")
      val B = json.Node("2")
      val ArB = json.Relationship("3", "hopfen", A.id, B.id)
      val jsonGraph: json.Graph = json.Graph(List(A, B), List(ArB))
      when(Query("some statement")).thenRespond(jsonGraph)

      val resultGraph = dbService.queryGraph(Query("some statement"))
      resultGraph must equalTo(json.GraphFactory(jsonGraph))
    }.pendingUntilFixed("stopped working after update to specs 3.6.3")

    "for list of queries in graphQuery return list of graphs" in todo

    "for list of queries in batchQuery return no data" in todo
  }
}

abstract class HttpMockSpecification extends Specification with HttpMockServer

class DbMock(mockServer: HttpMockServer) extends HttpMock(mockServer) {
  val dbService = new DbService
  dbService.restService = new RestService(requestUrl)

  override def requestUrl = s"${ mockServer.baseUri }${ super.requestUrl }"

  def when(query: Query): Stubbing = {
    val url = "/db/data/transaction/commit"
    when(post(url).build())
  }

  def post(url: String) = request().post(url)

  implicit def graphToJsonResponse(jsonGraph: json.Graph): ResponseDto = {
    json.Response(results = List(json.Result(Nil, List(json.Data(None, Some(jsonGraph))))))
  }

  implicit def jsonResponseToHttpResponse(jsonResponse: json.Response): ResponseDto = {
    import renesca.json.protocols.ResponseJsonProtocol._
    response().contentType("application/json").payload(jsonResponse.toJson.prettyPrint).build()
  }

}
