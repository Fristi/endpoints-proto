package itinere.server

import akka.http.scaladsl.model.{HttpMethods, HttpMethod => Method}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Directives}
import akka.stream.Materializer
import itinere.{HttpRequestAlgebra, InvariantFunctor, Tupler}
import shapeless._

import scala.concurrent.ExecutionContext

trait ServerRequest extends HttpRequestAlgebra with ServerUrl {

  implicit val materializer: Materializer
  implicit val executionContext: ExecutionContext

  override type HttpRequestHeaders[A] = Directive1[A]
  override type HttpRequestEntity[A] = Directive1[A]
  override type HttpRequest[A] = Directive1[A]
  override type HttpMethod = Method

  override def GET = HttpMethods.GET

  override def PUT = HttpMethods.PUT

  override def POST = HttpMethods.POST

  override def DELETE = HttpMethods.DELETE

  override def PATCH = HttpMethods.PATCH

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
}


