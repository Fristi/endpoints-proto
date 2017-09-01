package itinere

import shapeless._

trait EndpointAlgebra {
  type Request[A]
  type Response[A]
  type Endpoint[A, B]

  def endpoint[A, B](request: Request[A], response: Response[B]): Endpoint[A, B]
}

trait WithJsonCodec {
  type JsonCodecTypeClass[A]

  implicit def jsonCodec[A : JsonCodecTypeClass]: JsonCodec[A]
}

trait HttpJsonAlgebra { self: HttpRequestAlgebra with HttpResponseAlgebra with WithJsonCodec =>
  def jsonResponse[A : JsonCodec]: HttpResponseEntity[A]
  def jsonRequest[A : JsonCodec]: HttpRequestEntity[A]
}

trait HttpEndpointAlgebra extends EndpointAlgebra with HttpRequestAlgebra with HttpResponseAlgebra {
  type Request[A] = HttpRequest[A]
  type Response[A] = HttpResponse[A]
}

trait UrlAlgebra {

  /** A query string carrying an `A` information */
  type QueryString[A]

  /** Provides convenient methods on [[QueryString]]. */
  implicit class QueryStringOps[A](first: QueryString[A]) {
    /**
      * Convenient method to concatenate two [[QueryString]]s.
      *
      * {{{
      *   qs[Int]("foo") & qs[String]("baz")
      * }}}
      *
      * @param second `QueryString` to concatenate with this one
      * @tparam B Information carried by the second `QueryString`
      * @return A `QueryString` that carries both `A` and `B` information
      */
    final def & [B](second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
      combineQueryStrings(first, second)
  }

  /** Concatenates two `QueryString`s */
  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out]

  /**
    * Builds a `QueryString` with one parameter.
    *
    * @param name Parameter’s name
    * @tparam A Type of the value carried by the parameter
    */
  def qs[A](name: String)(implicit value: QueryStringParam[A]): QueryString[A]

  /**
    * Builds a `QueryString` with one optional parameter of type `A`.
    *
    * @param name Parameter’s name
    */
  def optQs[A](name: String)(implicit value: QueryStringParam[A]): QueryString[Option[A]]

  /**
    * A single query string parameter carrying an `A` information.
    */
  type QueryStringParam[A]

  /** Ability to define `String` query string parameters */
  implicit def stringQueryString: QueryStringParam[String]

  /** Ability to define `Int` query string parameters */
  implicit def intQueryString: QueryStringParam[Int]

  /** Query string parameter containing a `Long` value */
  implicit def longQueryString: QueryStringParam[Long]

  /**
    * An URL path segment carrying an `A` information.
    */
  type Segment[A]

  /** Ability to define `String` path segments */
  implicit def stringSegment: Segment[String]

  /** An URL path carrying an `A` information */
  type Path[A] <: Url[A]

  /** Convenient methods for [[Path]]s. */
  implicit class PathOps[A](first: Path[A]) {
    /** Chains this path with the `second` constant path segment */
    final def / (second: String)(implicit tupler: Tupler[A, HNil]): Path[tupler.Out] = chainPaths(first, staticPathSegment(second))
    /** Chains this path with the `second` path segment */
    final def / [B](second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] = chainPaths(first, second)
    /** Chains this path with the given [[QueryString]] */
    final def /? [B](qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] = urlWithQueryString(first, qs)
  }

  /** Builds a static path segment */
  def staticPathSegment(segment: String): Path[HNil]

  /** Builds a path segment carrying an `A` information */
  def segment[A](implicit s: Segment[A]): Path[A]

  /** Chains the two paths */
  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out]

  /**
    * An empty path.
    *
    * Useful to begin a path definition:
    *
    * {{{
    *   path / "foo" / segment[Int] /? qs[String]("bar")
    * }}}
    *
    */
  val path: Path[HNil] = staticPathSegment("")

  /**
    * An URL carrying an `A` information
    */
  type Url[A]

  /** Builds an URL from the given path and query string */
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
  type HttpRequestEntity[A]
  type HttpRequest[A]

  type HttpMethod

  def GET: HttpMethod
  def PUT: HttpMethod
  def POST: HttpMethod
  def DELETE: HttpMethod
  def PATCH: HttpMethod

  def emptyRequestHeaders: HttpRequestHeaders[HNil]
  def emptyRequestEntity: HttpRequestEntity[HNil]

  def request[A, B, C, AB](method: HttpMethod, url: Url[A], headers: HttpRequestHeaders[B] = emptyRequestHeaders, entity: HttpRequestEntity[C] = emptyRequestEntity)
                          (implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out]

  implicit val httpRequestHeadersInvariantFunctor: InvariantFunctor[HttpRequestHeaders]
  implicit val httpRequestEntityInvariantFunctor: InvariantFunctor[HttpRequestEntity]
  implicit val httpRequestInvariantFunctor: InvariantFunctor[HttpRequest]
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