package itinere

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.stream.ActorMaterializer
import argonaut.derive.{DerivedInstances, SingletonInstances}
import argonaut.{CodecJson, DecodeJson, EncodeJson, Parse}
import itinere.domain._
import itinere.server.{Server, ServerJson}

import scala.concurrent.Future

trait ArgonautShapeless extends SingletonInstances with DerivedInstances

trait ArgonautJsonCodec extends WithJsonCodec with ArgonautShapeless {
  override type JsonCodecTypeClass[A] = CodecJson[A]

  implicit def codecJson[A](implicit E: EncodeJson[A], D: DecodeJson[A]): CodecJson[A] =
    CodecJson(E.apply, D.apply)

  implicit def jsonCodec[A : CodecJson]: JsonCodec[A] = new JsonCodec[A] {
    override def encode(entity: A): String = implicitly[CodecJson[A]].encode(entity).nospaces

    override def decode(input: String): Either[String, A] =
      Parse.parse(input)
        .right
        .flatMap(json => implicitly[CodecJson[A]].decodeJson(json).fold[Either[String, A]]((err, _) => Left(err), Right.apply))
  }
}

trait Test extends HttpEndpointAlgebra with HttpJsonAlgebra with ArgonautJsonCodec {

  def listUsers: Endpoint[ListUserRequest, DomainResponse[List[User]]] = endpoint(
    request(GET, path / "test" /? optQs[String]("kind")).as[ListUserRequest],
    (
      response(400, entity = jsonResponse[Error]).as[BadRequest] |
        (response(404, entity = jsonResponse[Error]).as[NotFound] |
        (response(200, entity = jsonResponse[List[User]]).as[Success[List[User]]] | cnil))
    ).as[DomainResponse[List[User]]]
  )

}



object ServerApp extends App {

  implicit val system = ActorSystem("my-system")
  implicit val materializer = ActorMaterializer()
  // needed for the future flatMap/onComplete in the end
  implicit val executionContext = system.dispatcher

  object ServerImpl extends Server with Test with ServerJson {
    val routes = listUsers.implementedByAsync {
      case r if r.kind.contains("notFound") => Future.successful(NotFound(Error("notFound")))
      case r if r.kind.contains("badRequest") => Future.successful(BadRequest(Error("badRequest")))
      case _ => Future.successful(Success(List.empty))
    }
  }

  val bindingFuture = Http().bindAndHandle(ServerImpl.routes, "localhost", 8080)


}

object ClientApp extends App {



}