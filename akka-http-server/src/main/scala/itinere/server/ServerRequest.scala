package itinere.server

import akka.http.scaladsl.model.{HttpMethods, HttpMethod => Method}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Directives, MalformedHeaderRejection}
import akka.stream.Materializer
import itinere.{HttpRequestAlgebra, InvariantFunctor, Tupler}
import shapeless._

import scala.concurrent.ExecutionContext
import scala.util.Try

trait ServerRequest extends HttpRequestAlgebra with ServerUrl {

  implicit val materializer: Materializer
  implicit val executionContext: ExecutionContext

  override type HttpRequestHeaders[A] = Directive1[A]
  override type HttpRequestEntity[A] = Directive1[A]
  override type HttpRequest[A] = Directive1[A]

  override type HttpRequestHeaderValue[A] = String => Either[String, A]
  override type HttpMethod = Method

  override def GET = HttpMethods.GET

  override def PUT = HttpMethods.PUT

  override def POST = HttpMethods.POST

  override def DELETE = HttpMethods.DELETE

  override def PATCH = HttpMethods.PATCH

  override def requestHeader[A](name: String, description: Option[String] = None)(implicit V: (String) => Either[String, A]): Directive1[A] =
    headerValueByName(name).flatMap(h => V(h).fold(err => reject(MalformedHeaderRejection(name, err)), provide))

  override implicit def stringRequestHeader: (String) => Either[String, String] = str => Right(str)

  override implicit def intRequestHeader: (String) => Either[String, Int] = str => Try(str.toInt).fold(err => Left(err.getMessage), Right.apply)

  override def combineRequestHeaders[A, B](left: Directive1[A], right: Directive1[B])(implicit T: Tupler[A, B]): Directive1[T.Out] =
    joinDirectives(left, right)

  override def request[A, B, C, AB](method: Method, url: Url[A], headers: Directive1[B], entity: Directive1[C])
                                   (implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): Directive1[TO.Out] = {
    val methodDirective = convToDirective1(Directives.method(method))
    // we use Directives.pathPrefix to construct url directives, so now we close it
//    val urlDirective = joinDirectives(url.directive, convToDirective1(pathEndOrSingleSlash))

    joinDirectives(joinDirectives(joinDirectives(url.directive, headers), entity), methodDirective)
  }

  override def emptyRequestHeaders: Directive1[HNil] = convToDirective1(pass)

  override def emptyRequestEntity: Directive1[HNil] = convToDirective1(pass)

  val directiveFunctor = new InvariantFunctor[Directive1] {
    override def imap[A, B](fa: Directive1[A])(f: (A) => B)(g: (B) => A): Directive1[B] = fa.tmap(x => f(x._1))
  }

  override implicit val httpRequestHeadersInvariantFunctor: InvariantFunctor[Directive1] = directiveFunctor
  override implicit val httpRequestEntityInvariantFunctor: InvariantFunctor[Directive1] = directiveFunctor
  override implicit val httpRequestInvariantFunctor: InvariantFunctor[Directive1] = directiveFunctor
  override implicit val httpRequestHeaderInvariantFunctor: InvariantFunctor[Lambda[A => Function[String, Either[String, A]]]] = new InvariantFunctor[Lambda[A => Function[String, Either[String, A]]]] {
    override def imap[A, B](fa: Function[String, Either[String, A]])(f: (A) => B)(g: (B) => A): Function[String, Either[String, B]] = str => fa(str).map(f)
  }
}


