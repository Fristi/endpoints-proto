package itinere.example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import akka.http.scaladsl.server.Directives._
import fs2.{Strategy, Task}
import io.circe.Printer
import io.circe.generic.AutoDerivation
import itinere._
import itinere.client.{Client, ClientJson, ClientSettings}
import itinere.http4s.{Http4sServer, Http4sServerJson}
import itinere.json.circe.CirceJsonCodec
import itinere.server.{AkkaHttpServer, AkkaHttpServerJson}
import itinere.swagger.circe._
import itinere.swagger.{SwaggerApiInfo, SwaggerGen, SwaggerGenJson}
import org.http4s.util.StreamApp

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

trait ApiEndpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with CirceJsonCodec with AutoDerivation {

  def domainResponse[A](implicit E: JsonCodec[A], S: JsonSchema[A]): HttpResponse[DomainResponse[A]] =
    coproductResponseBuilder
      .add(response(404, entity = jsonResponse[Error](Some("Returned when a entity is not found"))).as[NotFound])
      .add(response(200, entity = jsonResponse[A](Some("Returned when everything is fine"))).as[Success[A]])
      .add(response(400, entity = jsonResponse[Error](Some("Returned when the user has provided a bad request"))).as[BadRequest])
      .as[DomainResponse[A]]

  def listUsers: Endpoint[ListUserRequest, DomainResponse[List[User]]] = endpoint(
    request = request(GET, path / "users" /? optQs[String]("kind", Some("The kind of user to filter"))).as[ListUserRequest],
    response = domainResponse[List[User]],
    description = Some("This endpoint will list users")
  )

  def addUser: Endpoint[AddUserRequest, DomainResponse[User]] = endpoint(
    request = request(POST, path / "users",
      headers =
        requestHeader[String]("X-Content-Type", Some("The content type")) ~
        requestHeader[String]("X-Real-IP", Some("The ip address")) ~
        requestHeader[Int]("X-Age", Some("The age of the request")),
      entity = jsonRequest[User](Some("The user to add"))
    ).as[AddUserRequest],
    response = domainResponse[User],
    description = Some("This endpoint will add a user")
  )
}

object AkkaServerApp extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  implicit val executionContext = system.dispatcher

  val gen = new SwaggerGen with SwaggerGenJson with ApiEndpoints
  val api = gen.api(SwaggerApiInfo("Simple API", "1.0.0", "A simple api"), "/")(gen.listUsers, gen.addUser)

  object ServerImpl extends AkkaHttpServer with ApiEndpoints with AkkaHttpServerJson {
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

object Http4sServerApp extends StreamApp {
  final class ServerImpl(strategy: Strategy) extends Http4sServer()(strategy) with ApiEndpoints with Http4sServerJson {
    val handlers = listUsers.implementedByAsync {
      case r if r.kind.contains("notFound") => Task.now(NotFound(Error("notFound")))
      case r if r.kind.contains("badRequest") => Task.now(BadRequest(Error("badRequest")))
      case _ => Task.now(Success(List(User("mark", 223), User("julien", 2323))))
    } orElse addUser.implementedByAsync { req =>
      Task.now(Success(req.user))
    }
  }

  override def stream(args: List[String]): fs2.Stream[Task, Nothing] = new ServerImpl(Strategy.sequential).server(8080)
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
  println(Await.result(HttpClient.addUser(AddUserRequest("application/json", "127.0.0.1", 3, User("mark", 2323))), 2.seconds))

}