package itinere.json.argonaut

import argonaut.{CodecJson, DecodeJson, EncodeJson, Parse}
import argonaut.derive.{DerivedInstances, SingletonInstances}
import itinere.{JsonCodec, WithJsonCodec}

trait ArgonautShapeless extends SingletonInstances with DerivedInstances

trait ArgonautJsonCodec extends WithJsonCodec with ArgonautShapeless {
  override type JsonCodecTypeClass[A] = CodecJson[A]

  implicit def codecJson[A](implicit E: EncodeJson[A], D: DecodeJson[A]): CodecJson[A] =
    CodecJson(E.apply, D.apply)

  implicit def jsonCodec[A : CodecJson]: JsonCodec[A] = new JsonCodec[A] {
    override def encode(entity: A): String = implicitly[CodecJson[A]].encode(entity).nospaces

    override def decode(input: String): Either[String, A] =
      Parse.parse(input)
        .right
        .flatMap(json => implicitly[CodecJson[A]].decodeJson(json).fold[Either[String, A]]((err, cursor) => Left(s"$err -> $cursor"), Right.apply))
  }
}