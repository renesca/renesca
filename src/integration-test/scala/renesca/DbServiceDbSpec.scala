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
class DbServiceDbSpec extends IntegrationSpecification {

  "DbService" should {

    "throw exception on Neo4j Error" in {
      db.batchQuery("this is invalid cypher syntax") must throwA[RuntimeException]
    }
  }

}

@RunWith(classOf[JUnitRunner])
class DbServiceWithMockDbSpec extends HttpMockSpecification {
  "DbService" should {
    "execute graph query" in new DbMock(this) {
      val A = json.Node("1")
      val B = json.Node("2")
      val ArB = json.Relationship("3", "hopfen", A.id, B.id)
      val jsonGraph: json.Graph = json.Graph(List(A, B), List(ArB))
      when(Query("some statement")).thenRespond(jsonGraph)

      val resultGraph = graphManager.dbService.queryGraph(Query("some statement"))
      resultGraph must equalTo(Graph(jsonGraph))
    }
  }
}

abstract class HttpMockSpecification extends Specification with HttpMockServer

class DbMock(mockServer: HttpMockServer) extends HttpMock(mockServer) {
  val graphManager = new GraphManager
  graphManager.dbService = new DbService
  graphManager.dbService.restService = new RestService(requestUrl)

  override def requestUrl = s"${mockServer.baseUri}${super.requestUrl}"

  def when(query: Query): Stubbing = {
    val url = "/db/data/transaction/commit"
    when(post(url).build())
  }

  def post(url: String) = request().post(url)

  implicit def graphToJsonResponse(jsonGraph: json.Graph): ResponseDto = {
    json.Response(List(json.Result(Nil, List(json.Data(None, Some(jsonGraph))))))
  }

  implicit def jsonResponseToHttpResponse(jsonResponse: json.Response): ResponseDto = {
    import renesca.json.protocols.ResponseJsonProtocol._
    response().contentType("application/json").payload(jsonResponse.toJson.prettyPrint).build()
  }

}