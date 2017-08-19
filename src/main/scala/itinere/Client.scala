package itinere

import akka.http.scaladsl.model
import akka.http.scaladsl.model.{HttpEntity, HttpMethod, HttpMethods, HttpEntity => Entity, HttpHeader => Header, HttpRequest => Req, HttpResponse => Resp}

import scala.concurrent.Future

trait Client extends Test {
  trait QueryString[A] {
    def encode(a: A): Option[String]
  }
  trait Segment[A] {
    def encode(a: A): String
  }

  trait Url[A] {
    def encode(a: A): String
  }

  trait Path[A] extends Url[A]

  override type QueryStringParam[A] = A => String
  override type Endpoint[A, B] = A => Future[B]
  override type HttpResponseHeaders = this.type
  override type HttpResponseEntity = this.type
  override type HttpResponse[A] = (Resp, Int) => Throwable Either A
  override type HttpRequestHeaders[A] =  (A, List[Header]) => List[Header]
  override type HttpRequestEntity[A] = (A, Req) => Req
  override type HttpRequest[A] = A => Future[Resp]


  override implicit val httpResponseCocartesian: CoCartesian[HttpResponse] = new CoCartesian[HttpResponse] {
    override def sum[A, B](fa: HttpResponse[A], fb: HttpResponse[B]): HttpResponse[Either[A, B]] =
      (resp, statusCode) => fa(resp, statusCode) match {
        case Left(errFa) =>
          fb(resp, statusCode) match {
            case Left(errFb) => Left(errFb)
            case Right(fbv) => Right(Right(fbv))
          }

        case Right(fav) => Right(Left(fav))
      }
  }


  override implicit val httpRequestInvariantFunctor: InvariantFunctor[Function[?, Future[Resp]]] = new InvariantFunctor[Function[?, Future[Resp]]] {
    override def imap[A, B](fa: Function[A, Future[Resp]])(f: (A) => B)(g: (B) => A): Function[B, Future[Resp]] = b => fa(g(b))
  }

  override type HttpMethod = Req => Req

  override def Get: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.GET)

  override def Put: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.PUT)

  override def Post: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.POST)

  override def Delete: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.DELETE)

  override def Patch: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.PATCH)
}
