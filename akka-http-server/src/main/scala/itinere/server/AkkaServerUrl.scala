package itinere.server

import akka.http.scaladsl.server.{Directive1, PathMatcher1, PathMatchers}
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.unmarshalling.{FromStringUnmarshaller, PredefinedFromStringUnmarshallers, Unmarshaller}
import itinere.{InvariantFunctor, Tupler, UrlAlgebra}
import shapeless.HNil

trait AkkaServerUrl extends UrlAlgebra {

  class Path[T](override val directive: Directive1[T]) extends Url[T](directive)

  class Url[T](val directive: Directive1[T])

  class QueryString[T](val directive: Directive1[T])

  type QueryStringParam[T] = FromStringUnmarshaller[T]

  type Segment[T] = PathMatcher1[T]


  def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] = {
    new Url(joinDirectives(path.directive, qs.directive))
  }

  //***************
  // Query strings
  //***************

  implicit def intQueryString: QueryStringParam[Int] = PredefinedFromStringUnmarshallers.intFromStringUnmarshaller

  implicit def stringQueryString: QueryStringParam[String] = Unmarshaller.identityUnmarshaller[String]

  implicit def longQueryString: QueryStringParam[Long] = PredefinedFromStringUnmarshallers.longFromStringUnmarshaller

  def qs[A](name: String, description: Option[String] = None)(implicit value: QueryStringParam[A]): QueryString[A] = {
    new QueryString[A](parameter(name.as[A]))
  }

  def optQs[A](name: String, description: Option[String] = None)(implicit value: QueryStringParam[A]): QueryString[Option[A]] = {
    new QueryString[Option[A]](parameter(name.as[A].?))
  }

  def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] = {
    new QueryString(joinDirectives(first.directive, second.directive))
  }

  // ********
  // Paths
  // ********

  implicit def intSegment: Segment[Int] = PathMatchers.IntNumber

  implicit def stringSegment: Segment[String] = PathMatchers.Segment

  implicit def longSegment: Segment[Long] = PathMatchers.LongNumber

  def segment[A](name: String, description: Option[String] = None)(implicit s: Segment[A]): Path[A] = {
    new Path(pathPrefix(s))
  }

  def staticPathSegment(segment: String): Path[HNil] = {
    val directive = if(segment.isEmpty) // We cannot use Directives.pathPrefix("") because it consumes also a leading slash
      pass
    else
      pathPrefix(segment)
    new Path(convToDirective1(directive))
  }

  def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] = {
    new Path(joinDirectives(first.directive, second.directive))
  }




  override implicit val segmentInvariantFunctor: InvariantFunctor[PathMatcher1] = new InvariantFunctor[PathMatcher1] {
    override def imap[A, B](fa: PathMatcher1[A])(f: (A) => B)(g: (B) => A): PathMatcher1[B] = fa.map(f)
  }
  override implicit val queryStringInvariantFunctor: InvariantFunctor[QueryString] = new InvariantFunctor[QueryString] {
    override def imap[A, B](fa: QueryString[A])(f: (A) => B)(g: (B) => A): QueryString[B] = new QueryString(fa.directive.map(f))
  }
  override implicit val pathInvariantFunctor: InvariantFunctor[Path] = new InvariantFunctor[Path] {
    override def imap[A, B](fa: Path[A])(f: (A) => B)(g: (B) => A): Path[B] = new Path(fa.directive.map(f))
  }
  override implicit val urlInvariantFunctor: InvariantFunctor[Url] = new InvariantFunctor[Url] {
    override def imap[A, B](fa: Url[A])(f: (A) => B)(g: (B) => A): Url[B] = new Url(fa.directive.map(f))
  }
}
