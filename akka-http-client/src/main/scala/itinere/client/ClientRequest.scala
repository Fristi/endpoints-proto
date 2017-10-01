package itinere.client

import akka.http.scaladsl.model
import akka.http.scaladsl.model.{HttpMethods, Uri, HttpHeader => Header, HttpRequest => Req, HttpResponse => Resp}
import akka.http.scaladsl.model.headers.RawHeader
import itinere.{HttpRequestAlgebra, InvariantFunctor, Tupler}
import shapeless.HNil

import scala.concurrent.Future

trait ClientRequest extends HttpRequestAlgebra with ClientUrls {
  self: Client =>
  override type HttpRequestHeaders[A] = (A, List[Header]) => List[Header]
  override type HttpRequestEntity[A] = (A, Req) => Req
  override type HttpRequest[A] = A => Future[Resp]
  override type HttpRequestHeaderValue[A] = A => String
  override type HttpMethod = Req => Req

  override def requestHeader[A](name: String, description: Option[String] = None)(implicit V: HttpRequestHeaderValue[A]): (A, List[Header]) => List[Header] =
    (a,headers) => headers :+ RawHeader(name, V(a))


  override implicit def stringRequestHeader: (String) => String = identity
  override implicit def intRequestHeader: (Int) => String = _.toString

  override def combineRequestHeaders[A, B](
    left: (A, List[Header]) => List[Header],
    right: (B, List[Header]) => List[Header])(implicit T: Tupler[A, B]): (T.Out, List[Header]) => List[Header] = (out, headers) => {
    val (a, b) = T.unapply(out)

    right(b, left(a, headers))
  }

  override def GET: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.GET)

  override def PUT: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.PUT)

  override def POST: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.POST)

  override def DELETE: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.DELETE)

  override def PATCH: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.PATCH)


  lazy val emptyRequestHeaders: HttpRequestHeaders[HNil] = (_, req) => req
  lazy val emptyRequestEntity: HttpRequestEntity[HNil] = (_, req) => req

  override implicit val httpRequestHeadersInvariantFunctor: InvariantFunctor[(?, List[Header]) => List[Header]] = new InvariantFunctor[(?, List[Header]) => List[Header]] {
    override def imap[A, B](fa: (A, List[Header]) => List[Header])(f: (A) => B)(g: (B) => A): (B, List[Header]) => List[Header] = (b, headers) => fa(g(b), headers)
  }
  override implicit val httpRequestEntityInvariantFunctor: InvariantFunctor[(?, Req) => Req] = new InvariantFunctor[(?, Req) => Req] {
    override def imap[A, B](fa: (A, Req) => Req)(f: (A) => B)(g: (B) => A): (B, Req) => Req = (b, req) => fa(g(b), req)
  }
  override implicit val httpRequestInvariantFunctor: InvariantFunctor[Function[?, Future[Resp]]] = new InvariantFunctor[Function[?, Future[Resp]]] {
    override def imap[A, B](fa: Function[A, Future[Resp]])(f: (A) => B)(g: (B) => A): Function[B, Future[Resp]] = (b) => fa(g(b))
  }

  override implicit val httpRequestHeaderInvariantFunctor: InvariantFunctor[Lambda[A => Function[A, String]]] = new InvariantFunctor[Lambda[A => Function[A, String]]] {
    override def imap[A, B](fa: Function[A, String])(f: (A) => B)(g: (B) => A): Function[B, String] = a => fa(g(a))
  }

  override def request[A, B, C, AB](method: (Req) => Req, url: Url[A], headers: (B, List[Header]) => List[Header], entity: (C, Req) => Req)
                                   (implicit tuplerAB: Tupler.Aux[A, B, AB], tuplerABC: Tupler[AB, C]): HttpRequest[tuplerABC.Out] =
    (abc: tuplerABC.Out) => {
      val (ab, c) = tuplerABC.unapply(abc)
      val (a, b) = tuplerAB.unapply(ab)
      val uri =
        if (self.settings.baseUri == Uri("/")) Uri(url.encode(a))
        else Uri(s"${self.settings.baseUri.path}${url.encode(a)}")

      val request = method(entity(c, Req(uri = uri)))
        .withHeaders(headers(b, List.empty))

      self.settings.requestExecutor(request)
    }
}
