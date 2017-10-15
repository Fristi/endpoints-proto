package itinere.http4s

import fs2._
import itinere.{CoCartesian, HttpResponseAlgebra, InvariantFunctor, Tupler}
import org.http4s._
import shapeless.{CNil, HNil}

trait Http4sServerResponse extends HttpResponseAlgebra {
  override type HttpResponseHeaders[A] = A => Headers
  override type HttpResponseEntity[A] = A => Stream[Task, Byte]
  override type HttpResponse[A] = A => Response

  override def emptyResponseHeaders: (HNil) => Headers = _ => Headers.empty

  override def emptyResponse: (HNil) => Stream[Task, Byte] = _ => Stream.empty

  override def cnil: (CNil) => Response = _ => Response(Status.Forbidden, body = Stream.empty)

  override def response[A, B](
    statusCode: Int,
    headers: (A) => Headers,
    entity: (B) => Stream[Task, Byte])(implicit T: Tupler[A, B]): (T.Out) => Response = out => {
    val (a,b) = T.unapply(out)
    val h = headers(a)
    val e = entity(b)
    val s = Status.fromInt(statusCode).getOrElse(sys.error(s"Invalid status code $statusCode"))

    Response(s, headers = h, body = e)
  }

  override implicit val httpResponseResponseHeadersInvariantFunctor: InvariantFunctor[Lambda[A => Function[A, Headers]]] = new InvariantFunctor[Function[?, Headers]] {
    override def imap[A, B](fa: Function[A, Headers])(f: (A) => B)(g: (B) => A): Function[B, Headers] = b => fa(g(b))
  }
  override implicit val httpResponseEntityInvariantFunctor: InvariantFunctor[Lambda[A => Function[A, Stream[Task, Byte]]]] = new InvariantFunctor[Function[?, Stream[Task, Byte]]] {
    override def imap[A, B](fa: Function[A, Stream[Task, Byte]])(f: (A) => B)(g: (B) => A): Function[B, Stream[Task, Byte]] = b => fa(g(b))
  }
  override implicit val httpResponseInvariantFunctor: InvariantFunctor[Lambda[A => Function[A, Response]]] = new InvariantFunctor[Function[?, Response]] {
    override def imap[A, B](fa: Function[A, Response])(f: (A) => B)(g: (B) => A): Function[B, Response] = b => fa(g(b))
  }
  override implicit val httpResponseCocartesian: CoCartesian[Lambda[A => Function[A, Response]]] = new CoCartesian[Function[?, Response]] {
    override def sum[A, B](fa: Function[A, Response], fb: Function[B, Response]): Function[Either[A, B], Response] = {
      case Left(a) => fa(a)
      case Right(b) => fb(b)
    }
  }
}
