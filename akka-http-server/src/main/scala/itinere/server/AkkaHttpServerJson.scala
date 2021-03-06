package itinere.server

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.server.Directive1
import akka.http.scaladsl.server.Directives.{extractRequest, onSuccess}
import itinere.{HttpJsonAlgebra, JsonCodec, JsonSchema, WithJsonCodec}

import scala.concurrent.Future
import scala.concurrent.duration._

trait AkkaHttpServerJson extends HttpJsonAlgebra { self: AkkaHttpServer with WithJsonCodec =>

  override def jsonResponse[A: JsonCodec : JsonSchema](description: Option[String] = None): HttpResponseEntity[A] = (x, resp) =>
    resp.withEntity(ContentTypes.`application/json`, implicitly[JsonCodec[A]].encode(x))

  override def jsonRequest[A: JsonCodec : JsonSchema](description: Option[String] = None): Directive1[A] = extractRequest.flatMap { r =>

    def extractJson = for {
      strictEntity <- r.entity.toStrict(2.seconds)
      result <- implicitly[JsonCodec[A]].decode(strictEntity.data.utf8String).fold(err => Future.failed(new Throwable(err)), Future.successful)
    } yield result

    onSuccess(extractJson)
  }
}
