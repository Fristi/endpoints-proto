package itinere.swagger

import cats._
import cats.data.Writer
import cats.implicits._
import itinere.{Field, Schema}

case class SchemaManifest(manifest: Map[String, Schema])

object SchemaManifest {
  val empty = SchemaManifest(Map.empty[String, Schema])

  def apply(init: (String, Schema)): SchemaManifest = SchemaManifest(Map(init))

  implicit val monoid: Monoid[SchemaManifest] = new Monoid[SchemaManifest] {
    override def empty: SchemaManifest = SchemaManifest.empty
    override def combine(x: SchemaManifest, y: SchemaManifest): SchemaManifest = SchemaManifest(x.manifest ++ y.manifest)
  }
}



object SchemaTraverals {


  def toReferenceTree(schm: Schema): Writer[SchemaManifest, Schema] = schm match {
    case Schema.Object(fields) =>
      fields.traverse[Writer[SchemaManifest, ?], Field] { field =>
        field.content match {
          case Schema.Type(id, schema) => toReferenceTree(schema).flatMap(s => Writer(SchemaManifest(id -> s), Field(field.key, field.required, Schema.Ref(id))))
          case s => toReferenceTree(s).map(ss => Field(field.key, field.required, ss))
        }
      } map (f => Schema.Object(f))
    case Schema.Choice(choices) =>
      choices.traverse[Writer[SchemaManifest, ?], Schema] {
        case Schema.Type(id, Schema.Nil) => Writer(SchemaManifest(id -> Schema.EmptyObject), Schema.Ref(id))
        case Schema.Type(id, s) => toReferenceTree(s).flatMap(ss => Writer(SchemaManifest(id -> ss), Schema.Ref(id)))
        case s => toReferenceTree(s)
      } map (c => Schema.Choice(c))

    case Schema.Array(bound, uniqueItems, schema) =>
      schema match {
        case Schema.Type(id, s) => toReferenceTree(s).flatMap(ss => Writer(SchemaManifest(id -> ss), Schema.Array(bound, uniqueItems, Schema.Ref(id))))
        case s => toReferenceTree(s).map(ss => Schema.Array(bound, uniqueItems, ss))
      }

    case Schema.Tuple(entries) => entries.traverse[Writer[SchemaManifest, ?], Schema](toReferenceTree).map(Schema.Tuple.apply)
    case Schema.Type(id, schema) => toReferenceTree(schema).flatMap(ss => Writer(SchemaManifest(id -> ss), Schema.Ref(id)))

    case s: Schema.Ref => Writer(SchemaManifest.empty, s)
    case s: Schema.Integer => Writer(SchemaManifest.empty, s)
    case s: Schema.Value => Writer(SchemaManifest.empty, s)
    case s: Schema.Number => Writer(SchemaManifest.empty, s)
    case s: Schema.Enum => Writer(SchemaManifest.empty, s)

    case Schema.Boolean => Writer(SchemaManifest.empty, Schema.Boolean)
    case Schema.Nil => Writer(SchemaManifest.empty, Schema.Nil)
    case Schema.EmptyObject => Writer(SchemaManifest.empty, Schema.EmptyObject)
  }

}
