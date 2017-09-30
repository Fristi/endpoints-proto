package itinere.example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.http.scaladsl.server.Directives._
import io.circe.Printer
import io.circe.generic.AutoDerivation
import itinere._
import itinere.client.{Client, ClientJson, ClientSettings}
import itinere.json.circe.CirceJsonCodec
import itinere.server.{Server, ServerJson}
import itinere.swagger.circe._
import itinere.swagger.{SwaggerApiInfo, SwaggerGen, SwaggerGenJson}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait ApiEndpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with CirceJsonCodec with AutoDerivation {

  def domainResponse[A](implicit E: JsonCodec[A], S: JsonSchema[A]): HttpResponse[DomainResponse[A]] =
    coproductResponseBuilder
      .add(response(404, entity = jsonResponse[Error]).as[NotFound])
      .add(response(200, entity = jsonResponse[A]).as[Success[A]])
      .add(response(400, entity = jsonResponse[Error]).as[BadRequest])
      .as[DomainResponse[A]]

  def listUsers: Endpoint[ListUserRequest, DomainResponse[List[User]]] = endpoint(
    request(GET, path / "users" /? optQs[String]("kind")).as[ListUserRequest],
    domainResponse[List[User]]
  )

  def addUser: Endpoint[AddUserRequest, DomainResponse[User]] = endpoint(
    request(POST, path / "users", entity = jsonRequest[User]).as[AddUserRequest],
    domainResponse[User]
  )
}

object ServerApp extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  val gen = new SwaggerGen with SwaggerGenJson with ApiEndpoints
  val api = gen.api(SwaggerApiInfo("Simple API", "1.0.0", "A simple api"), "/")(gen.listUsers, gen.addUser)

  object ServerImpl extends Server with ApiEndpoints with ServerJson {
    val routes = listUsers.implementedByAsync {
      case r if r.kind.contains("notFound") => Future.successful(NotFound(Error("notFound")))
      case r if r.kind.contains("badRequest") => Future.successful(BadRequest(Error("badRequest")))
      case _ => Future.successful(Success(List(User("mark", 223), User("julien", 2323))))
    } ~ addUser.implementedByAsync { req =>
      Future.successful(Success(req.user))
    }
  }


  val swagger = pathPrefix("swagger-ui") {
    getFromResourceDirectory("META-INF/resources/webjars/swagger-ui/3.0.18") ~
      pathSingleSlash(get(redirect("index.html", StatusCodes.TemporaryRedirect)))
  } ~ path("api") {
    get {
      complete(
        HttpResponse(
          entity = encoderSwaggerApi(api).pretty(new Printer(preserveOrder = true, dropNullKeys = true, ""))
        )
      )
    }
  }

  val bindingFuture = Http().bindAndHandle(ServerImpl.routes ~ swagger, "localhost", 8080)
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