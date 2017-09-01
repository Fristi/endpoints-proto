package itinere.client

import akka.http.scaladsl.model.{Uri, HttpEntity => Entity, HttpRequest => Req, HttpResponse => Resp}
import akka.stream.Materializer
import itinere.{HttpEndpointAlgebra, Profunctor}

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future}

case class ClientSettings(
                           baseUri: Uri,
                           toStrictTimeout: FiniteDuration,
                           stringContentExtractor: Entity.Strict => String,
                           requestExecutor: Req => Future[Resp]
                         )

abstract class Client(val settings: ClientSettings)(implicit val ec: ExecutionContext, val m: Materializer)
  extends HttpEndpointAlgebra
    with ClientUrls
    with ClientRequest
    with ClientResponse {
  override type Endpoint[A, B] = A => Future[B]

  def endpoint[A, B](request: Request[A], response: Response[B]): Endpoint[A, B] =
    a =>
      for {
        resp <- request(a)
        result <- response(resp -> resp.status.intValue())
      } yield result

  implicit val profunctorEndpoint: Profunctor[Endpoint] = new Profunctor[Endpoint] {
    override def dimap[A, B, C, D](fab: (A) => Future[B])(f: (C) => A)(g: (B) => D): (C) => Future[D] = c => fab(f(c)).map(g)
  }
}
