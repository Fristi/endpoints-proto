package itinere

import java.net.URLEncoder

import akka.http.scaladsl.model
import akka.http.scaladsl.model.{ContentTypes, HttpMethods, Uri, HttpEntity => Entity, HttpHeader => Header, HttpRequest => Req, HttpResponse => Resp}
import shapeless.{CNil, HNil}

import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import java.nio.charset.StandardCharsets.UTF_8

import akka.stream.Materializer

case class Settings(
                     baseUri: Uri,
                     toStrictTimeout: FiniteDuration,
                     stringContentExtractor: Entity.Strict => String,
                     requestExecutor: Req => Future[Resp]
                   )

abstract class Client(val settings: Settings)(implicit val ec: ExecutionContext, val m: Materializer) extends Test with ClientUrls with ClientRequest with ClientResponse {
  override type Endpoint[A, B] = A => Future[B]

  def endpoint[A, B](request: Request[A], response: Response[B]): Endpoint[A, B] =
    a =>
      for {
        resp <- request(a)
        result <- response(resp -> resp.status.intValue())
      } yield result

  override implicit val profunctorEndpoint: Profunctor[Endpoint] = new Profunctor[Endpoint] {
    override def dimap[A, B, C, D](fab: (A) => Future[B])(f: (C) => A)(g: (B) => D): (C) => Future[D] = c => fab(f(c)).map(g)
  }
}

trait ClientResponse extends HttpResponseAlgebra { self: Client =>

  override type HttpResponseHeaders[A] = Resp => A
  override type HttpResponseEntity[A] = Resp => Future[A]
  override type HttpResponse[A] = PartialFunction[(Resp, Int), Future[A]]

  override def jsonResponse[A : JsonCodec]: (model.HttpResponse) => Future[A] = model => for {
    strictEntity <- model.entity.toStrict(2.seconds)
    result <- implicitly[JsonCodec[A]]
      .decode(strictEntity.data.utf8String)
      .fold(x => Future.failed(new Throwable(x)), Future.successful)
  } yield result


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
    case (resp, code) if code == statusCode =>
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
      case (resp, code) => fa(resp -> code).map(f)
    }
  }
}

trait ClientRequest extends HttpRequestAlgebra with ClientUrls { self: Client =>
  override type HttpRequestHeaders[A] =  (A, List[Header]) => List[Header]
  override type HttpRequestEntity[A] = (A, Req) => Req
  override type HttpRequest[A] = A => Future[Resp]

  override type HttpMethod = Req => Req

  override def GET: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.GET)

  override def PUT: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.PUT)

  override def POST: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.POST)

  override def DELETE: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.DELETE)

  override def PATCH: (model.HttpRequest) => model.HttpRequest = _.withMethod(HttpMethods.PATCH)

  override def jsonRequest[A: JsonCodec]: (A, model.HttpRequest) => model.HttpRequest = (entity, req) =>
    req.withEntity(ContentTypes.`application/json`, implicitly[JsonCodec[A]].encode(entity))

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

  override def request[A, B, C, AB](method: (Req) => Req, url: Url[A], headers: (B, List[Header]) => List[Header], entity: (C, Req) => Req)
                                   (implicit tuplerAB: Tupler.Aux[A, B, AB], tuplerABC: Tupler[AB, C]): HttpRequest[tuplerABC.Out] =
    (abc: tuplerABC.Out) => {
      val (ab, c) = tuplerABC.unapply(abc)
      val (a, b) = tuplerAB.unapply(ab)
      val uri =
        if(self.settings.baseUri == Uri("/")) Uri(url.encode(a))
        else Uri(s"${self.settings.baseUri.path}${url.encode(a)}")

      val request = method(entity(c, Req(uri = uri)))
        .withHeaders(headers(b, List.empty))

      self.settings.requestExecutor(request)
    }
}


trait ClientUrls extends UrlAlgebra {

  val utf8Name = UTF_8.name()

  trait QueryString[A] {
    def encodeQueryString(a: A): Option[String]
  }

  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
    (ab: tupler.Out) => {
      val (a, b) = tupler.unapply(ab)

      (first.encodeQueryString(a), second.encodeQueryString(b)) match {
        case (Some(left), Some(right)) => Some(s"$left&$right")
        case (Some(left), None) => Some(left)
        case (None, Some(right)) => Some(right)
        case (None, None) => None
      }
    }

  def qs[A](name: String)(implicit value: QueryStringParam[A]): QueryString[A] =
    a => Some(s"$name=${value.apply(a)}")

  def optQs[A](name: String)(implicit value: QueryStringParam[A]): QueryString[Option[A]] = {
    case Some(a) => qs[A](name).encodeQueryString(a)
    case None => None
  }

  type QueryStringParam[A] = A => String

  implicit lazy val stringQueryString: QueryStringParam[String] = s => URLEncoder.encode(s, utf8Name)

  implicit lazy val intQueryString: QueryStringParam[Int] = i => i.toString

  implicit lazy val longQueryString: QueryStringParam[Long] = i => i.toString


  trait Segment[A] {
    def encode(a: A): String
  }

  implicit lazy val stringSegment: Segment[String] = (s: String) => URLEncoder.encode(s, utf8Name)

  implicit lazy val intSegment: Segment[Int] = (i: Int) => i.toString

  implicit lazy val longSegment: Segment[Long] = (i: Long) => i.toString


  trait Path[A] extends Url[A]

  def staticPathSegment(segment: String) = (_: HNil) => segment

  def segment[A](implicit s: Segment[A]): Path[A] = a => s.encode(a)

  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] =
    (ab: tupler.Out) => {
      val (a, b) = tupler.unapply(ab)
      first.encode(a) ++ "/" ++ second.encode(b)
    }


  trait Url[A] {
    def encode(a: A): String
  }

  def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] =
    (ab: tupler.Out) => {
      val (a, b) = tupler.unapply(ab)

      qs.encodeQueryString(b) match {
        case Some(q) => s"${path.encode(a)}?$q"
        case None => path.encode(a)
      }
    }

  override implicit val segmentInvariantFunctor: InvariantFunctor[Segment] = new InvariantFunctor[Segment] {
    override def imap[A, B](fa: Segment[A])(f: (A) => B)(g: (B) => A): Segment[B] = b => fa.encode(g(b))
  }
  override implicit val queryStringInvariantFunctor: InvariantFunctor[QueryString] = new InvariantFunctor[QueryString] {
    override def imap[A, B](fa: QueryString[A])(f: (A) => B)(g: (B) => A): QueryString[B] = b => fa.encodeQueryString(g(b))
  }
  override implicit val pathInvariantFunctor: InvariantFunctor[Path] = new InvariantFunctor[Path] {
    override def imap[A, B](fa: Path[A])(f: (A) => B)(g: (B) => A): Path[B] = b => fa.encode(g(b))
  }
  override implicit val urlInvariantFunctor: InvariantFunctor[Url] = new InvariantFunctor[Url] {
    override def imap[A, B](fa: Url[A])(f: (A) => B)(g: (B) => A): Url[B] = b => fa.encode(g(b))
  }
}