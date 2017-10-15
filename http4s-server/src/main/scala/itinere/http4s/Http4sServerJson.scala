package itinere.http4s

import fs2.{Task, text}
import itinere.{HttpJsonAlgebra, JsonCodec, JsonSchema, WithJsonCodec}

trait Http4sServerJson extends HttpJsonAlgebra { self: Http4sServer with WithJsonCodec =>
  override def jsonResponse[A: JsonCodec : JsonSchema](description: Option[String]): HttpResponseEntity[A] = entity =>
    fs2.Stream[Task, String](implicitly[JsonCodec[A]].encode(entity)) through text.utf8Encode

  override def jsonRequest[A: JsonCodec : JsonSchema](description: Option[String]): HttpRequestEntity[A] = new HttpRequestEntity[A] {
    override def decode(stream: fs2.Stream[Task, Byte]): Task[Either[String, A]] =
      stream.through(text.utf8Decode).runLog.map(t => implicitly[JsonCodec[A]].decode(t.mkString))
  }
}
