package itinere

import java.time.{LocalDate, LocalDateTime}

import shapeless._
import shapeless.labelled.FieldType
import enum._

import scala.reflect.ClassTag

sealed trait Schema { self =>
  def <|>(other: Schema): Schema = (self, other) match {
    case (Schema.Choice(a), Schema.Choice(b)) => Schema.Choice(a ++ b)
    case (Schema.Choice(a), v) => Schema.Choice(a :+ v)
    case (v, Schema.Choice(b)) => Schema.Choice(b :+ v)
    case (a, b) => Schema.Choice(List(a, b))
  }

  def <+>(other: Schema): Schema = (self, other) match {
    case (Schema.Object(a), Schema.Object(b)) => Schema.Object(a ++ b)
    case (Schema.Tuple(a), Schema.Tuple(b)) => Schema.Tuple(a ++ b)
    case (Schema.Tuple(a), v) => Schema.Tuple(a :+ v)
    case (v, Schema.Tuple(b)) => Schema.Tuple(b :+ v)
    case (a, Schema.Nil) => a
    case (Schema.Nil, b) => b
    case (a, b) => Schema.Tuple(List(a, b))
  }
}

case class Field(key: String, required: Boolean, content: Schema)
case class LengthBound(lowerBound: Option[Int], upperBound: Option[Int])
case class Bound(lowerBound: Option[Int], upperBound: Option[Int])

sealed abstract class IntegerType(val format: String)

object IntegerType {
  case object Int32 extends IntegerType("int32")
  case object Int64 extends IntegerType("int64")
}

sealed abstract class NumberType(val format: String)

object NumberType {
  case object Float extends NumberType("float")
  case object Double extends NumberType("double")
}

sealed abstract class StringType(val format: String)

object StringType {
  case object DateTime extends StringType("date-time")
  case object Email extends StringType("email")
  case object Hostname extends StringType("hostname")
  case object IPV4 extends StringType("ipv4")
  case object IPV6 extends StringType("ipv6")
  case object Uri extends StringType("uri")
}

object Schema {
  final case class Enum(choices: Set[String]) extends Schema
  final case class Choice(choices: List[Schema]) extends Schema
  final case class Type(id: String, schema: Schema) extends Schema
  final case class Ref(id: String) extends Schema
  final case class Object(fields: List[Field]) extends Schema
  final case class Array(lengthBound: LengthBound, uniqueItems: Boolean, schema: Schema) extends Schema
  final case class Tuple(entries: List[Schema]) extends Schema
  final case class Value(lengthBound: LengthBound, format: Option[StringType]) extends Schema
  final case class Number(bound: Bound, numberType: NumberType) extends Schema
  final case class Integer(bound: Bound, integerType: IntegerType) extends Schema
  case object Boolean extends Schema
  case object Nil extends Schema
  case object EmptyObject extends Schema
}

trait JsonSchema[A] {
  def schema: Schema
}


trait LowerPrioJsonSchema1 {
  protected def unbounded = Bound(None, None)
  protected def unboundedLength = LengthBound(None, None)

  implicit val hnil: JsonSchema[HNil] = JsonSchema[HNil](Schema.Nil)

  implicit def hcons[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K], H: Lazy[JsonSchema[H]], T: JsonSchema[T]): JsonSchema[FieldType[K, H] :: T] = new JsonSchema[FieldType[K, H] :: T] {
    override def schema: Schema =
      Schema.Object(List(Field(key = key.value.name, required = true, content = H.value.schema))) <+> T.schema
  }


  implicit val cnil: JsonSchema[CNil] = JsonSchema[CNil](Schema.Nil)

  implicit def ccons[K <: Symbol, H, T <: Coproduct](implicit key: Witness.Aux[K], H: Lazy[JsonSchema[H]], T: JsonSchema[T]): JsonSchema[FieldType[K, H] :+: T] = new JsonSchema[:+:[FieldType[K, H], T]] {
    override def schema: Schema = Schema.Choice(List(H.value.schema)) <|> T.schema
  }

  implicit def generic[T, R](implicit gen: LabelledGeneric.Aux[T, R], R: Lazy[JsonSchema[R]], TT: ClassTag[T]): JsonSchema[T] = new JsonSchema[T] {
    override def schema: Schema = Schema.Type(TT.runtimeClass.getSimpleName, R.value.schema)
  }
}

object JsonSchema extends LowerPrioJsonSchema1 {
  def apply[A](s: Schema) = new JsonSchema[A] {
    override def schema: Schema = s
  }

  def by[A, B](f: B => A)(implicit C: JsonSchema[A]): JsonSchema[B] = new JsonSchema[B] {
    override def schema: Schema = C.schema
  }

  implicit def labels[E : Enum]: JsonSchema[E] = new JsonSchema[E] {
    override def schema: Schema = Schema.Enum(implicitly[Enum[E]].labels)
  }

  implicit def list[A : JsonSchema]: JsonSchema[List[A]] =
    JsonSchema[List[A]](Schema.Array(unboundedLength, false, implicitly[JsonSchema[A]].schema))

  implicit def seq[A : JsonSchema]: JsonSchema[Seq[A]] =
    JsonSchema[Seq[A]](Schema.Array(unboundedLength, false, implicitly[JsonSchema[A]].schema))

  implicit def set[A : JsonSchema]: JsonSchema[Set[A]] =
    JsonSchema[Set[A]](Schema.Array(unboundedLength, true, implicitly[JsonSchema[A]].schema))

  implicit val int: JsonSchema[Int] = JsonSchema[Int](Schema.Integer(unbounded, IntegerType.Int32))
  implicit val long: JsonSchema[Long] = JsonSchema[Long](Schema.Integer(unbounded, IntegerType.Int64))
  implicit val float: JsonSchema[Float] = JsonSchema[Float](Schema.Number(unbounded, NumberType.Float))
  implicit val double: JsonSchema[Double] = JsonSchema[Double](Schema.Number(unbounded, NumberType.Double))
  implicit val bigDecimal: JsonSchema[BigDecimal] = JsonSchema[BigDecimal](Schema.Number(unbounded, NumberType.Double))
  implicit val bool: JsonSchema[Boolean] = JsonSchema[Boolean](Schema.Boolean)
  implicit val string: JsonSchema[String] = JsonSchema[String](Schema.Value(unboundedLength, None))
  implicit val localDate: JsonSchema[LocalDate] = JsonSchema[LocalDate](Schema.Value(unboundedLength, Some(StringType.DateTime)))
  implicit val localDateTime: JsonSchema[LocalDateTime] = JsonSchema[LocalDateTime](Schema.Value(unboundedLength, Some(StringType.DateTime)))

  implicit def hconsOption[K <: Symbol, H, T <: HList](implicit key: Witness.Aux[K], H: JsonSchema[H], T: JsonSchema[T]): JsonSchema[FieldType[K, Option[H]] :: T] = new JsonSchema[FieldType[K, Option[H]] :: T] {
    override val schema: Schema =
      Schema.Object(List(Field(key = key.value.name, required = false, content = H.schema))) <+> T.schema
  }

  def apply[T](implicit T: JsonSchema[T]) = T
}