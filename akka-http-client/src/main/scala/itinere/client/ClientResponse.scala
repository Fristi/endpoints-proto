package itinere.client

import akka.http.scaladsl.model
import akka.http.scaladsl.model.{HttpResponse => Resp}
import itinere.{CoCartesian, HttpResponseAlgebra, InvariantFunctor, Tupler}
import shapeless.{CNil, HNil}

import scala.concurrent.Future

trait ClientResponse extends HttpResponseAlgebra {
  self: Client =>

  override type HttpResponseHeaders[A] = Resp => A
  override type HttpResponseEntity[A] = Resp => Future[A]
  override type HttpResponse[A] = PartialFunction[(Resp, Int), Future[A]]


  override def cnil: PartialFunction[(Resp, Int), Future[CNil]] = {
    case (resp, code) =>
      Future.failed(new Throwable(s"No response handler for $code - $resp"))
  }

  override implicit val httpResponseCocartesian: CoCartesian[HttpResponse] = new CoCartesian[HttpResponse] {
    override def sum[A, B](fa: HttpResponse[A], fb: HttpResponse[B]): HttpResponse[Either[A, B]] =
      fa.andThen(_.map(Left.apply)) orElse fb.andThen(_.map(Right.apply))
  }

  override def emptyResponseHeaders: (model.HttpResponse) => HNil = _ => HNil

  override def emptyResponse: (model.HttpResponse) => Future[HNil] = _ => Future.successful(HNil)

  override def response[A, B](statusCode: Int, headers: (Resp) => A, entity: (Resp) => Future[B])
                             (implicit T: Tupler[A, B]): PartialFunction[(Resp, Int), Future[T.Out]] = {
    case (resp, status) if status == statusCode =>
      entity(resp).map(b => T(headers(resp), b))
  }

  override implicit val httpResponseResponseHeadersInvariantFunctor: InvariantFunctor[HttpResponseHeaders] = new InvariantFunctor[HttpResponseHeaders] {
    override def imap[A, B](fa: Resp => A)(f: (A) => B)(g: (B) => A): Resp => B = resp => f(fa(resp))
  }
  override implicit val httpResponseEntityInvariantFunctor: InvariantFunctor[HttpResponseEntity] = new InvariantFunctor[HttpResponseEntity] {
    override def imap[A, B](fa: (Resp) => Future[A])(f: (A) => B)(g: (B) => A): (Resp) => Future[B] = resp => fa(resp).map(f)
  }
  override implicit val httpResponseInvariantFunctor: InvariantFunctor[HttpResponse] = new InvariantFunctor[HttpResponse] {
    override def imap[A, B](fa: PartialFunction[(Resp, Int), Future[A]])(f: (A) => B)(g: (B) => A): PartialFunction[(Resp, Int), Future[B]] = {
      case (resp, code) if fa.isDefinedAt(resp -> code) => fa(resp -> code).map(f)
    }
  }
}
