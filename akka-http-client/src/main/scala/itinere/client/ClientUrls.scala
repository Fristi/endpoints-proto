package itinere.client

import java.net.URLEncoder
import java.nio.charset.StandardCharsets.UTF_8

import itinere.{InvariantFunctor, Tupler, UrlAlgebra}
import shapeless.HNil

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
