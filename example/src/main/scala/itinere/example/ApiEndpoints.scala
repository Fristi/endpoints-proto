package itinere.example

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, Uri}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.{Sink, Source}
import itinere._
import itinere.client.{Client, ClientJson, ClientSettings}
import itinere.swagger.{SwaggerApi, SwaggerGen, SwaggerGenJson}
import itinere.json.argonaut.ArgonautJsonCodec
import itinere.server.{Server, ServerJson}
import itinere.swagger.circe._

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}




trait ApiEndpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with ArgonautJsonCodec {

  def domainResponse[A](implicit E: JsonCodec[A], S: JsonSchema[A]): HttpResponse[DomainResponse[A]] =
    coproductResponseBuilder
      .add(response(404, entity = jsonResponse[Error]).as[NotFound])
      .add(response(200, entity = jsonResponse[A]).as[Success[A]])
      .add(response(400, entity = jsonResponse[Error]).as[BadRequest])
      .as[DomainResponse[A]]

  def listUsers: Endpoint[ListUserRequest, DomainResponse[List[User]]] = endpoint(
    request(GET, path / "test" / segment[String]("user") /? optQs[String]("kind")).as[ListUserRequest],
    domainResponse[List[User]]
  )

}

//case class ServiceId(id: String) extends AnyVal
//case class ServiceAccountId(id: String) extends AnyVal
//case class MerchantTransactionId(id: String) extends AnyVal
//case class MerchantTransaction(id: String) extends AnyVal
//
//sealed trait SdiResponse[+A]
//object SdiResponse {
//  final case class Ok[A](value: A) extends SdiResponse[A]
//  final case class Created[A](value: A) extends SdiResponse[A]
//  object NoContent extends SdiResponse[Nothing]
//  final case class BadRequest(error: Error) extends SdiResponse[Nothing]
//  final case class Forbidden(error: Error) extends SdiResponse[Nothing]
//  final case class NotFound(error: Error) extends SdiResponse[Nothing]
//}
//
//final case class AvailabilityCheckRequest(serviceId: ServiceId, serviceAccountId: Option[ServiceAccountId])
//final case class CreateMerchantTransactionRequest(serviceId: ServiceId, serviceAccountId: ServiceAccountId, merchantTransaction: MerchantTransaction)
//final case class GetMerchantTransactionRequest(serviceId: ServiceId, serviceAccountId: ServiceAccountId, merchantTransactionId: MerchantTransactionId)
//final case class UpdateMerchantTransactionRequest(serviceId: ServiceId, serviceAccountId: ServiceAccountId, merchantTransactionId: MerchantTransactionId, merchantTransaction: MerchantTransaction)

//trait SdiEndpoints extends HttpEndpointAlgebra with HttpJsonAlgebra with ArgonautJsonCodec {
//
//  private val serviceId = stringSegment.imap(ServiceId.apply)(_.id)
//  private val serviceAccountId = stringSegment.imap(ServiceAccountId.apply)(_.id)
//  private val merchantTransactionId = stringSegment.imap(MerchantTransactionId.apply)(_.id)
//
//  private implicit val serviceAccountIdQs = stringQueryString.imap(ServiceAccountId.apply)(_.id)
//
//  def sdiResponse[A](implicit E: JsonCodec[A]): HttpResponse[SdiResponse[A]] =
//    coproductResponseBuilder
//      .add(response(200, entity = jsonResponse[A]).as[SdiResponse.Ok[A]])
//      .add(response(200, entity = jsonResponse[A]).as[SdiResponse.Created[A]])
//      .add(response(204).as[SdiResponse.NoContent.type])
//      .add(response(400, entity = jsonResponse[Error]).as[SdiResponse.BadRequest])
//      .add(response(403, entity = jsonResponse[Error]).as[SdiResponse.Forbidden])
//      .add(response(404, entity = jsonResponse[Error]).as[SdiResponse.NotFound])
//      .as[SdiResponse[A]]
//
//
//  def availability: Endpoint[AvailabilityCheckRequest, SdiResponse[Unit]] = endpoint(
//    request(GET, path / "services" / segment(serviceId) / "availability" /? optQs[ServiceAccountId]("serviceAccountId")).as[AvailabilityCheckRequest],
//    sdiResponse[Unit]
//  )
//
//  def createTransaction: Endpoint[CreateMerchantTransactionRequest, SdiResponse[MerchantTransaction]] = endpoint(
//    request(POST, path / "services" / segment(serviceId) / "serviceAccounts" / segment(serviceAccountId) / "merchantTransactions", entity = jsonRequest[MerchantTransaction]).as[CreateMerchantTransactionRequest],
//    sdiResponse[MerchantTransaction]
//  )
//
//  def getTransaction: Endpoint[GetMerchantTransactionRequest, SdiResponse[MerchantTransaction]] = endpoint(
//    request(GET, path / "services" / segment(serviceId) / "serviceAccounts" / segment(serviceAccountId) / "merchantTransactions" / segment(merchantTransactionId)).as[GetMerchantTransactionRequest],
//    sdiResponse[MerchantTransaction]
//  )
//
//  def updateTransaction: Endpoint[UpdateMerchantTransactionRequest, SdiResponse[MerchantTransaction]] = endpoint(
//    request(PUT, path / "services" / segment(serviceId) / "serviceAccounts" / segment(serviceAccountId) / "merchantTransactions" / segment(merchantTransactionId), entity = jsonRequest[MerchantTransaction]).as[UpdateMerchantTransactionRequest],
//    sdiResponse[MerchantTransaction]
//  )
//}



object ServerApp extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  object ServerImpl extends Server with ApiEndpoints with ServerJson {
    val routes = listUsers.implementedByAsync {
      case r if r.kind.contains("notFound") => Future.successful(NotFound(Error("notFound")))
      case r if r.kind.contains("badRequest") => Future.successful(BadRequest(Error("badRequest")))
      case _ => Future.successful(Success(List(User("mark", 223), User("julien", 2323))))
    }
  }

  val bindingFuture = Http().bindAndHandle(ServerImpl.routes, "localhost", 8080)


}

object DoclessApp extends App {

  class DoclessTest extends SwaggerGen with ApiEndpoints with SwaggerGenJson

  val t = new DoclessTest

//  println(t.listUsers)

  val (manifest, operation) = t.listUsers.toReferenceTree.run

  val api = SwaggerApi(List(operation), manifest, "/")

  println(encoderSwaggerApi(api))
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


  println(Await.result(HttpClient.listUsers(ListUserRequest("test", Some("badRequest"))), 2.seconds))
  println(Await.result(HttpClient.listUsers(ListUserRequest("test", Some("notFound"))), 2.seconds))
  println(Await.result(HttpClient.listUsers(ListUserRequest("test", None)), 2.seconds))

}