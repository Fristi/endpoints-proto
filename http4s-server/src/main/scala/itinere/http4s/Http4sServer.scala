package itinere.http4s

import fs2.{Strategy, Task}
import itinere.HttpEndpointAlgebra
import org.http4s.server.blaze._
import org.http4s.{HttpService, Request => Req, Response => Resp}

abstract class Http4sServer(implicit val strategy: Strategy)
  extends HttpEndpointAlgebra
    with Http4sServerResponse
    with Http4sServerRequest
{
  case class Endpoint[A, B](request: Request[A], response: Response[B]) {
    def implementedByAsync(implementation: A => Task[B]): PartialFunction[Req, Task[Resp]] =
      request.andThen(_.flatMap(implementation).map(response.apply))
  }

  override def endpoint[A, B](request: PartialFunction[Req, Task[A]], response: (B) => Resp, description: Option[String]): Endpoint[A, B] = {
    Endpoint(request, response)
  }

  val handlers: PartialFunction[Req, Task[Resp]]

  def server(port: Int): fs2.Stream[Task, Nothing] =
    BlazeBuilder
      .bindHttp(port)
      .mountService(HttpService(handlers), "/")
      .serve
}