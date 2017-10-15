package itinere.http4s

import fs2.{Stream, Task}
import itinere.{HttpRequestAlgebra, InvariantFunctor, Tupler}
import org.http4s.{Request => Req, Headers, Method}
import org.http4s.util.CaseInsensitiveString
import shapeless.HNil
import cats.implicits._

import scala.util.Try

trait Http4sServerRequest extends HttpRequestAlgebra with Http4sServerUrl { self: Http4sServer =>
  trait HttpRequestHeaders[A] {
    def decode(headers: Headers): Either[String, A]
  }

  trait HttpRequestHeaderValue[A] {
    def decode(string: String): Either[String, A]
  }

  trait HttpRequestEntity[A] {
    def decode(stream: Stream[Task, Byte]): Task[Either[String, A]]
  }

  type HttpRequest[A] = PartialFunction[Req, Task[A]]

  override type HttpMethod = Method

  override def GET = Method.GET
  override def PUT = Method.PUT
  override def POST = Method.POST
  override def DELETE = Method.DELETE
  override def PATCH = Method.PATCH

  override implicit def intRequestHeader: HttpRequestHeaderValue[Int] = new HttpRequestHeaderValue[Int] {
    override def decode(string: String): Either[String, Int] = Try(string.toInt).fold(err => Left(err.getMessage), Right.apply)
  }

  override implicit def stringRequestHeader: HttpRequestHeaderValue[String] = new HttpRequestHeaderValue[String] {
    override def decode(string: String): Either[String, String] = Right(string)
  }

  override def requestHeader[A](name: String, description: Option[String])(implicit V: HttpRequestHeaderValue[A]): HttpRequestHeaders[A] = new HttpRequestHeaders[A] {
    override def decode(headers: Headers): Either[String, A] =
      headers.get(CaseInsensitiveString(name))
        .fold[Either[String, String]](Left(s"No header found for $name"))(x => Right(x.value))
        .flatMap(V.decode)
  }

  override def combineRequestHeaders[A, B](left: HttpRequestHeaders[A], right: HttpRequestHeaders[B])(implicit T: Tupler[A, B]): HttpRequestHeaders[T.Out] = new HttpRequestHeaders[T.Out] {
    override def decode(headers: Headers): Either[String, T.Out] = for {
      a <- left.decode(headers)
      b <- right.decode(headers)
    } yield T(a, b)
  }

  override def emptyRequestHeaders: HttpRequestHeaders[HNil] = new HttpRequestHeaders[HNil] {
    override def decode(headers: Headers): Either[String, HNil] = Right(HNil)
  }

  override def emptyRequestEntity: HttpRequestEntity[HNil] = new HttpRequestEntity[HNil] {
    override def decode(stream: Stream[Task, Byte]): Task[Either[String, HNil]] =
      stream.runLog.map(s =>
        if(s.isEmpty) Right(HNil)
        else Left("Expected empty body, but given")
      )
  }

  override def request[A, B, C, AB](
    method: Method,
    url: Url[A],
    headers: HttpRequestHeaders[B],
    entity: HttpRequestEntity[C])(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): HttpRequest[TO.Out] =
    Function.unlift[Req, Task[TO.Out]] { req =>
      if(req.method === method) {
        for {
          a <- url.decode(req.uri).toOption
          b <- headers.decode(req.headers).toOption
        } yield entity
          .decode(req.body)
          .flatMap(_.fold(err => Task.fail(new Throwable(err)), Task.now))
          .map(c => TO.apply(T.apply(a, b), c))
      } else {
        None
      }
    }

  override implicit val httpRequestHeaderInvariantFunctor: InvariantFunctor[HttpRequestHeaderValue] = new InvariantFunctor[HttpRequestHeaderValue] {
    override def imap[A, B](fa: HttpRequestHeaderValue[A])(f: (A) => B)(g: (B) => A): HttpRequestHeaderValue[B] = new HttpRequestHeaderValue[B] {
      override def decode(string: String): Either[String, B] = fa.decode(string).map(f)
    }
  }
  override implicit val httpRequestHeadersInvariantFunctor: InvariantFunctor[HttpRequestHeaders] = new InvariantFunctor[HttpRequestHeaders] {
    override def imap[A, B](fa: HttpRequestHeaders[A])(f: (A) => B)(g: (B) => A): HttpRequestHeaders[B] = new HttpRequestHeaders[B] {
      override def decode(headers: Headers): Either[String, B] = fa.decode(headers).map(f)
    }
  }
  override implicit val httpRequestEntityInvariantFunctor: InvariantFunctor[HttpRequestEntity] = new InvariantFunctor[HttpRequestEntity] {
    override def imap[A, B](fa: HttpRequestEntity[A])(f: (A) => B)(g: (B) => A): HttpRequestEntity[B] = new HttpRequestEntity[B] {
      override def decode(stream: Stream[Task, Byte]): Task[Either[String, B]] = fa.decode(stream).map(_.map(f))
    }
  }
  override implicit val httpRequestInvariantFunctor: InvariantFunctor[HttpRequest] = new InvariantFunctor[HttpRequest] {
    override def imap[A, B](fa: HttpRequest[A])(f: (A) => B)(g: (B) => A): HttpRequest[B] = {
      case r: Req if fa.isDefinedAt(r) => fa(r).map(f)
    }
  }
}