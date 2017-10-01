package itinere

import shapeless._

trait EndpointAlgebra {
  type Request[A]
  type Response[A]
  type Endpoint[A, B]

  def endpoint[A, B](request: Request[A], response: Response[B], description: Option[String] = None): Endpoint[A, B]
}

trait HttpEndpointAlgebra extends EndpointAlgebra with HttpRequestAlgebra with HttpResponseAlgebra {
  type Request[A] = HttpRequest[A]
  type Response[A] = HttpResponse[A]
}

trait UrlAlgebra {

  type QueryString[A]
  type QueryStringParam[A]
  type Segment[A]
  type Path[A] <: Url[A]
  type Url[A]

  implicit class QueryStringOps[A](first: QueryString[A]) {
    final def & [B](second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
      combineQueryStrings(first, second)
  }

  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out]
  def qs[A](name: String, description: Option[String] = None)(implicit value: QueryStringParam[A]): QueryString[A]
  def optQs[A](name: String, description: Option[String] = None)(implicit value: QueryStringParam[A]): QueryString[Option[A]]

  implicit def stringQueryString: QueryStringParam[String]
  implicit def intQueryString: QueryStringParam[Int]
  implicit def longQueryString: QueryStringParam[Long]

  implicit def stringSegment: Segment[String]

  implicit class PathOps[A](first: Path[A]) {
    final def / (second: String)(implicit tupler: Tupler[A, HNil]): Path[tupler.Out] = chainPaths(first, staticPathSegment(second))
    final def / [B](second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] = chainPaths(first, second)
    final def /? [B](qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] = urlWithQueryString(first, qs)
  }

  def staticPathSegment(segment: String): Path[HNil]

  def segment[A](name: String, description: Option[String] = None)(implicit s: Segment[A]): Path[A]

  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out]

  val path: Path[HNil] = staticPathSegment("")

  def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out]

  implicit val segmentInvariantFunctor: InvariantFunctor[Segment]
  implicit val queryStringInvariantFunctor: InvariantFunctor[QueryString]
  implicit val pathInvariantFunctor: InvariantFunctor[Path]
  implicit val urlInvariantFunctor: InvariantFunctor[Url]
}

trait HttpResponseAlgebra {
  type HttpResponseHeaders[A]
  type HttpResponseEntity[A]
  type HttpResponse[A]

  final class CoproductHttpResponseBuilder[B <: Coproduct](coproduct: HttpResponse[B]) {
    def add[A](resp: HttpResponse[A]): CoproductHttpResponseBuilder[A :+: B] = {
      val newCoproduct = httpResponseCocartesian.sum(resp, coproduct).imap {
        case Left(l) => Inl(l)
        case Right(r) => Inr(r)
      } {
        case Inl(l) => Left(l)
        case Inr(r) => Right(r)
      }

      new CoproductHttpResponseBuilder(newCoproduct)
    }
    def as[A](implicit T: Transformer[HttpResponse, B, A]) = T(coproduct)
  }

  def coproductResponseBuilder = new CoproductHttpResponseBuilder(cnil)

  def emptyResponseHeaders: HttpResponseHeaders[HNil]
  def emptyResponse: HttpResponseEntity[HNil]
  def cnil: HttpResponse[CNil]

  def response[A, B](statusCode: Int, headers: HttpResponseHeaders[A] = emptyResponseHeaders, entity: HttpResponseEntity[B] = emptyResponse)(implicit T: Tupler[A, B]): HttpResponse[T.Out]

  implicit val httpResponseResponseHeadersInvariantFunctor: InvariantFunctor[HttpResponseHeaders]
  implicit val httpResponseEntityInvariantFunctor: InvariantFunctor[HttpResponseEntity]
  implicit val httpResponseInvariantFunctor: InvariantFunctor[HttpResponse]
  implicit val httpResponseCocartesian: CoCartesian[HttpResponse]
}

trait HttpRequestAlgebra extends UrlAlgebra {
  type HttpRequestHeaders[A]
  type HttpRequestHeaderValue[A]
  type HttpRequestEntity[A]
  type HttpRequest[A]

  type HttpMethod

  def GET: HttpMethod
  def PUT: HttpMethod
  def POST: HttpMethod
  def DELETE: HttpMethod
  def PATCH: HttpMethod

  implicit def stringRequestHeader: HttpRequestHeaderValue[String]
  implicit def intRequestHeader: HttpRequestHeaderValue[Int]

  implicit class RichHttpRequestHeaders[A](val left: HttpRequestHeaders[A]) {
    def ~[B](right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] =
      combineRequestHeaders(left, right)(T)
  }

  def requestHeader[A](name: String, description: Option[String] = None)(implicit V: HttpRequestHeaderValue[A]): HttpRequestHeaders[A]

  def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out]

  def emptyRequestHeaders: HttpRequestHeaders[HNil]
  def emptyRequestEntity: HttpRequestEntity[HNil]

  def request[A, B, C, AB](method: HttpMethod, url: Url[A], headers: HttpRequestHeaders[B] = emptyRequestHeaders, entity: HttpRequestEntity[C] = emptyRequestEntity)
                          (implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out]

  implicit val httpRequestHeaderInvariantFunctor: InvariantFunctor[HttpRequestHeaderValue]
  implicit val httpRequestHeadersInvariantFunctor: InvariantFunctor[HttpRequestHeaders]
  implicit val httpRequestEntityInvariantFunctor: InvariantFunctor[HttpRequestEntity]
  implicit val httpRequestInvariantFunctor: InvariantFunctor[HttpRequest]
}


trait WithJsonCodec {
  type JsonCodecTypeClass[A]

  implicit def jsonCodec[A : JsonCodecTypeClass]: JsonCodec[A]
}

trait HttpJsonAlgebra { self: HttpRequestAlgebra with HttpResponseAlgebra =>
  def jsonResponse[A : JsonCodec : JsonSchema](description: Option[String] = None): HttpResponseEntity[A]
  def jsonRequest[A : JsonCodec : JsonSchema](description: Option[String] = None): HttpRequestEntity[A]
}

trait JsonCodec[A] {
  def encode(entity: A): String
  def decode(input: String): String Either A
}

object JsonCodec {
  implicit val invariant = new InvariantFunctor[JsonCodec] {
    override def imap[A, B](fa: JsonCodec[A])(f: (A) => B)(g: (B) => A): JsonCodec[B] = new JsonCodec[B] {
      override def encode(entity: B): String = fa.encode(g(entity))
      override def decode(input: String): Either[String, B] = fa.decode(input).map(f)
    }
  }
}

trait CoCartesian[F[_]] {
  def sum[A, B](fa: F[A], fb: F[B]): F[Either[A, B]]
}

trait InvariantFunctor[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}

trait Profunctor[F[_,_]] {
  def dimap[A, B, C, D](fab: F[A, B])(f: C => A)(g: B => D): F[C, D]
}