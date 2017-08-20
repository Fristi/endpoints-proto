package itinere.server

import akka.http.scaladsl.server._
import itinere.{Profunctor, Test}

import scala.concurrent.Future

abstract class Server extends Test with ServerResponse with ServerRequest {

  case class Endpoint[A, B](request: Request[A], response: Response[B]) {
    def implementedByAsync(implementation: A => Future[B]): Route = request { arguments =>
      Directives.onComplete(implementation(arguments)) {
        case scala.util.Success(result) => response(result)
        case scala.util.Failure(ex) => Directives.complete(ex)
      }
    }
  }

  override implicit val profunctorEndpoint: Profunctor[Endpoint] = new Profunctor[Endpoint] {
    override def dimap[A, B, C, D](fab: Endpoint[A, B])(f: (C) => A)(g: (B) => D): Endpoint[C, D] = ???
  }


}

