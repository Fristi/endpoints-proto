package itinere.client

import akka.http.scaladsl.model
import akka.http.scaladsl.model.ContentTypes
import itinere.{HttpJsonAlgebra, JsonCodec, JsonSchema, WithJsonCodec}

import scala.concurrent.Future
import scala.concurrent.duration._

trait ClientJson extends HttpJsonAlgebra {
  self: Client with WithJsonCodec =>

  override def jsonRequest[A: JsonCodec : JsonSchema](description: Option[String] = None): (A, model.HttpRequest) => model.HttpRequest = (entity, req) =>
    req.withEntity(ContentTypes.`application/json`, implicitly[JsonCodec[A]].encode(entity))

  override def jsonResponse[A: JsonCodec : JsonSchema](description: Option[String] = None): (model.HttpResponse) => Future[A] = model => for {
    strictEntity <- model.entity.toStrict(2.seconds) //TODO make configurable
    data = strictEntity.data.utf8String
    result <- implicitly[JsonCodec[A]]
      .decode(data)
      .fold(x => Future.failed(new Throwable(x)), Future.successful)
  } yield result
}
