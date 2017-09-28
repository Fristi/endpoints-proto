package itinere.json.circe

import io.circe._
import io.circe.parser._
import itinere.{JsonCodec, WithJsonCodec}

trait Codec[A] {
  val encoder: Encoder[A]
  val decoder: Decoder[A]
}

trait CirceJsonCodec extends WithJsonCodec {
  override type JsonCodecTypeClass[A] = Codec[A]

  implicit def codecJson[A](implicit E: Encoder[A], D: Decoder[A]): Codec[A] =
    new Codec[A] {
      val encoder = E
      val decoder = D
    }

  implicit def jsonCodec[A : Codec]: JsonCodec[A] = new JsonCodec[A] {
    override def encode(entity: A): String = implicitly[Codec[A]].encoder(entity).noSpaces

    override def decode(input: String): Either[String, A] =
      parse(input)
        .left.map(_.message)
        .right
        .flatMap(json => implicitly[Codec[A]].decoder.decodeJson(json).fold[Either[String, A]](x => Left(x.message), Right.apply))
  }
}