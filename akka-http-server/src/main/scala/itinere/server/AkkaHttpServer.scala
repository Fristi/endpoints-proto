package itinere.server

import akka.http.scaladsl.server._
import akka.stream.Materializer
import itinere.HttpEndpointAlgebra

import scala.concurrent.{ExecutionContext, Future}

abstract class AkkaHttpServer(implicit val materializer: Materializer, val executionContext: ExecutionContext)
  extends HttpEndpointAlgebra
    with AkkaServerResponse
    with AkkaServerRequest
{

  case class Endpoint[A, B](request: Request[A], response: Response[B]) {
    def implementedByAsync(implementation: A => Future[B]): Route = request { arguments =>
      Directives.onComplete(implementation(arguments)) {
        case scala.util.Success(result) => response(result)
        case scala.util.Failure(ex) => Directives.complete(ex)
      }
    }
  }

  override def endpoint[A, B](request: Directive1[A], response: (B) => Route, description: Option[String] = None): Endpoint[A, B] = Endpoint(request, response)
}

