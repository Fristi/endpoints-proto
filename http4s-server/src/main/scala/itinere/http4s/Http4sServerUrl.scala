package itinere.http4s

import cats.Monad
import cats.implicits._
import itinere.{InvariantFunctor, Tupler, UrlAlgebra}
import org.http4s._
import shapeless.HNil

import scala.util.Try

trait Http4sServerUrl extends UrlAlgebra {

  sealed trait UriDecodeResult[+A] { self =>

    def fold[B](ifFailure: String => B, ifSuccess: (A, Uri) => B): B = self match {
      case UriDecodeResult.Success(v, uri) => ifSuccess(v, uri)
      case UriDecodeResult.Failure(err) => ifFailure(err)
    }

    def toOption: Option[A] =
      fold(_ => None, (v, uri) => if(uri.path == "") Some(v) else None)

    def toEither: Either[String, A] =
      fold(Left.apply, (v, uri) => if(uri.path == "") Right(v) else Left(s"Uri.path should be empty, but it's not: ${uri.path}"))

    def map[B](f: A => B): UriDecodeResult[B] = self match {
      case UriDecodeResult.Failure(err) => UriDecodeResult.Failure(err)
      case UriDecodeResult.Success(v, remainder) => UriDecodeResult.Success(f(v), remainder)
    }
  }

  object UriDecodeResult {
    case class Success[A](result: A, remainder: Uri) extends UriDecodeResult[A]
    case class Failure(error: String) extends UriDecodeResult[Nothing]
  }

  trait UriDecoder[A] {
    def decode(uri: Uri): UriDecodeResult[A]
  }

  object UriDecoder {
    implicit val monad: Monad[UriDecoder] = new Monad[UriDecoder] {
      override def pure[A](x: A): UriDecoder[A] = new UriDecoder[A] {
        override def decode(uri: Uri): UriDecodeResult[A] = UriDecodeResult.Success(x, uri)
      }

      override def flatMap[A, B](fa: UriDecoder[A])(f: (A) => UriDecoder[B]): UriDecoder[B] = new UriDecoder[B] {
        override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri) match {
          case UriDecodeResult.Success(v, remainder) => f(v).decode(remainder)
          case UriDecodeResult.Failure(err) => UriDecodeResult.Failure(err)
        }
      }

      override def tailRecM[A, B](a: A)(f: (A) => UriDecoder[Either[A, B]]): UriDecoder[B] = flatMap(f(a)) {
        case Left(v) => tailRecM(v)(f)
        case Right(v) => pure(v)
      }
    }
  }

  type Path[A] = UriDecoder[A]
  type Url[A] = UriDecoder[A]
  type QueryString[A] = UriDecoder[A]

  trait QueryStringParam[A] {
    def decode(string: String): Either[String, A]
  }

  trait Segment[A] {
    def decode(string: String): Either[String, A]
  }


  override def combineQueryStrings[A, B](first: QueryString[A], second: QueryString[B])(implicit tupler: Tupler[A, B]): QueryString[tupler.Out] =
    for {
      a <- first
      b <- second
    } yield tupler(a, b)

  override def qs[A](name: String, description: Option[String])(implicit value: QueryStringParam[A]): QueryString[A] = new UriDecoder[A] {
    override def decode(uri: Uri): UriDecodeResult[A] =
      uri.query
        .params
        .get(name)
        .fold[Either[String, String]](Left("No such param"))(Right.apply)
        .flatMap(value.decode)
        .fold[UriDecodeResult[A]](err => UriDecodeResult.Failure(err), v => UriDecodeResult.Success(v, uri))
  }

  override def optQs[A](name: String, description: Option[String])(implicit value: QueryStringParam[A]): QueryString[Option[A]] = new QueryString[Option[A]] {
    override def decode(uri: Uri): UriDecodeResult[Option[A]] =
      uri.query.params.get(name) match {
        case None => UriDecodeResult.Success(None, uri)
        case Some(v) => value.decode(v) match {
          case Left(err) => UriDecodeResult.Failure(err)
          case Right(vv) => UriDecodeResult.Success(Some(vv), uri)
        }
      }
  }

  override implicit def stringQueryString: QueryStringParam[String] = new QueryStringParam[String] {
    override def decode(string: String): Either[String, String] = Right(string)
  }

  override implicit def intQueryString: QueryStringParam[Int] = new QueryStringParam[Int] {
    override def decode(string: String): Either[String, Int] = Try(string.toInt).fold(err => Left(err.getMessage), Right.apply)
  }

  override implicit def longQueryString: QueryStringParam[Long] = new QueryStringParam[Long] {
    override def decode(string: String): Either[String, Long] = Try(string.toLong).fold(err => Left(err.getMessage), Right.apply)
  }

  override implicit def stringSegment: Segment[String] = new Segment[String] {
    override def decode(string: String): Either[String, String] = Right(string)
  }

  override def staticPathSegment(segment: String): Path[HNil] = new Path[HNil] {
    override def decode(uri: Uri): UriDecodeResult[HNil] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[HNil]](UriDecodeResult.Failure("Path is empty!"))(s =>
        if(segment == s) UriDecodeResult.Success(HNil, uri.copy(path = path.tail.mkString("/")))
        else UriDecodeResult.Failure(s"Segment `$s` didn't equal to `$segment`")
      )
    }

  }

  override def segment[A](name: String, description: Option[String])(implicit SD: Segment[A]): Path[A] = new Path[A] {
    override def decode(uri: Uri): UriDecodeResult[A] = {
      val path = uri.path.split('/')

      path.headOption.fold[UriDecodeResult[A]](UriDecodeResult.Failure("Path is empty!")) { s =>
        SD.decode(s) match {
          case Left(err) => UriDecodeResult.Failure(err)
          case Right(v) => UriDecodeResult.Success(v, uri.copy(path = path.tail.mkString("/")))
        }
      }
    }

  }

  override def chainPaths[A, B](first: Path[A], second: Path[B])(implicit tupler: Tupler[A, B]): Path[tupler.Out] =
    for {
      a <- first
      b <- second
    } yield tupler(a, b)

  override def urlWithQueryString[A, B](path: Path[A], qs: QueryString[B])(implicit tupler: Tupler[A, B]): Url[tupler.Out] =
    for {
      a <- path
      b <- qs
    } yield tupler(a, b)

  override implicit val segmentInvariantFunctor: InvariantFunctor[Segment] = new InvariantFunctor[Segment] {
    override def imap[A, B](fa: Segment[A])(f: (A) => B)(g: (B) => A): Segment[B] = new Segment[B] {
      override def decode(string: String): Either[String, B] = fa.decode(string).map(f)
    }
  }
  override implicit val queryStringInvariantFunctor: InvariantFunctor[QueryString] = new InvariantFunctor[QueryString] {
    override def imap[A, B](fa: QueryString[A])(f: (A) => B)(g: (B) => A): QueryString[B] = new QueryString[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri).map(f)
    }
  }
  override implicit val pathInvariantFunctor: InvariantFunctor[Path] = new InvariantFunctor[Path] {
    override def imap[A, B](fa: Path[A])(f: (A) => B)(g: (B) => A): Path[B] = new Path[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri).map(f)
    }
  }
  override implicit val urlInvariantFunctor: InvariantFunctor[Url] = new InvariantFunctor[Url] {
    override def imap[A, B](fa: Url[A])(f: (A) => B)(g: (B) => A): Url[B] = new Url[B] {
      override def decode(uri: Uri): UriDecodeResult[B] = fa.decode(uri).map(f)
    }
  }
}