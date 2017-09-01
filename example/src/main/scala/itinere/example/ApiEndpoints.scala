package itinere.example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import itinere.client.{Client, ClientJson, ClientSettings}
import itinere.json.argonaut.ArgonautJsonCodec
import itinere.server.{Server, ServerJson}
import itinere._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}




trait ApiEndpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with ArgonautJsonCodec {

  def listUsers: Endpoint[ListUserRequest, DomainResponse[List[User]]] = endpoint(
    request(GET, path / "test" /? optQs[String]("kind")).as[ListUserRequest],
    (
      response(404, entity = jsonResponse[Error]).as[NotFound] |
        (response(200, entity = jsonResponse[List[User]]).as[Success[List[User]]] |
        (response(400, entity = jsonResponse[Error]).as[BadRequest] |
        cnil))
    ).as[DomainResponse[List[User]]]
  )

}



object ServerApp extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  object ServerImpl extends Server with ApiEndpoints with ServerJson {
    val routes = listUsers.implementedByAsync {
      case r if r.kind.contains("notFound") => Future.successful(NotFound(Error("notFound")))
      case r if r.kind.contains("badRequest") => Future.successful(BadRequest(Error("badRequest")))
      case _ => Future.successful(Success(List(User("mark"), User("julien"))))
    }
  }

  val bindingFuture = Http().bindAndHandle(ServerImpl.routes, "localhost", 8080)


}

object ClientApp extends App {
  implicit val system = ActorSystem()
  implicit val materializer = ActorMaterializer()
  implicit val ec = system.dispatcher

  val poolClientFlow = Http().cachedHostConnectionPool[Int]("localhost", 8080)

  def pipeline(req: HttpRequest): Future[HttpResponse] =
    Source.single(req -> 1).via(poolClientFlow)
      .map(_._1.get)
      .runWith(Sink.head)


  val settings = ClientSettings(
    baseUri = Uri("/"),
    toStrictTimeout = 2.seconds,
    stringContentExtractor = _.data.utf8String,
    requestExecutor = pipeline
  )

  object HttpClient extends Client(settings) with ClientJson with ApiEndpoints


  println(Await.result(HttpClient.listUsers(ListUserRequest(Some("badRequest"))), 2.seconds))
  println(Await.result(HttpClient.listUsers(ListUserRequest(Some("notFound"))), 2.seconds))
  println(Await.result(HttpClient.listUsers(ListUserRequest(None)), 2.seconds))

}