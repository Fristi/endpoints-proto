package itinere.server

import akka.http.scaladsl.model.StatusCodes.InternalServerError
import akka.http.scaladsl.model.{StatusCodes, HttpResponse => Resp}
import akka.http.scaladsl.server.Directives.complete
import akka.http.scaladsl.server.Route
import itinere.{CoCartesian, HttpResponseAlgebra, InvariantFunctor, Tupler}
import shapeless.{CNil, HNil}

import scala.util.{Left, Right}

trait AkkaServerResponse extends HttpResponseAlgebra {
  override type HttpResponse[A] = A => Route
  override type HttpResponseEntity[A] = (A, Resp) => Resp
  override type HttpResponseHeaders[A] = (A, Resp) => Resp


  override implicit val httpResponseCocartesian: CoCartesian[HttpResponse] = new CoCartesian[HttpResponse] {
    override def sum[A, B](fa: (A) => Route, fb: (B) => Route) = {
      case Left(left) => fa(left)
      case Right(right) => fb(right)
    }
  }

  override def cnil: (CNil) => Route = _ => complete(InternalServerError -> "Unmapped entity")

  override def response[A, B](statusCode: Int, headers: (A, Resp) => Resp, entity: (B, Resp) => Resp)
                             (implicit T: Tupler[A, B]): (T.Out) => Route = ab => {
    val (a,b) = T.unapply(ab)
    val resp = Resp(StatusCodes.custom(statusCode, "No reason"))

    complete(entity(b, headers(a, resp)))
  }

  override def emptyResponseHeaders: (HNil, Resp) => Resp = (_, resp) => resp

  override def emptyResponse: (HNil, Resp) => Resp = (_, resp) => resp

  override implicit val httpResponseResponseHeadersInvariantFunctor: InvariantFunctor[(?, Resp) => Resp] = new InvariantFunctor[(?, Resp) => Resp] {
    override def imap[A, B](fa: (A, Resp) => Resp)(f: (A) => B)(g: (B) => A): (B, Resp) => Resp = (b, resp) => fa(g(b), resp)
  }
  override implicit val httpResponseEntityInvariantFunctor: InvariantFunctor[(?, Resp) => Resp] = new InvariantFunctor[(?, Resp) => Resp] {
    override def imap[A, B](fa: (A, Resp) => Resp)(f: (A) => B)(g: (B) => A): (B, Resp) => Resp = (b, resp) => fa(g(b), resp)
  }
  override implicit val httpResponseInvariantFunctor: InvariantFunctor[Function[?, Route]] = new InvariantFunctor[Function[?, Route]] {
    override def imap[A, B](fa: Function[A, Route])(f: (A) => B)(g: (B) => A): Function[B, Route] = b => fa(g(b))
  }
}
