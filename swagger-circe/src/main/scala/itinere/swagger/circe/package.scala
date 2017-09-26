package itinere.swagger

import io.circe.{Encoder, Json}
import itinere.Schema

package object circe {

  private def schemaToJson(schm: Schema): Json = schm match {
    case Schema.EmptyObject => Json.obj("properties" -> Json.arr())
    case Schema.Object(fields) =>
      Json.obj(
        "type" -> Json.fromString("object"),
        "properties" -> Json.obj(fields.map(x => x.key -> schemaToJson(x.content)) : _*),
        "required" -> Json.arr(fields.filter(_.required).map(x => Json.fromString(x.key)) : _*)
      )
    case Schema.Choice(choices) => Json.obj("anyOf" -> Json.arr(choices.map(schemaToJson) : _*))
    case Schema.Array(bound, uniqueItems, schema) =>
      Json.obj(
        "type" -> Json.fromString("array"),
        "uniqueItems" -> Json.fromBoolean(uniqueItems),
        "items" -> schemaToJson(schema)
      ) deepMerge Json.fromFields(bound.lowerBound.map(x => "minItems" -> Json.fromInt(x)) ++ bound.upperBound.map(x => "maxItems" -> Json.fromInt(x)))
    case Schema.Boolean => Json.obj("type" -> Json.fromString("boolean"))
    case Schema.Enum(choices) => Json.obj("type" -> Json.fromString("string"), "enum" -> Json.arr(choices.map(Json.fromString).toSeq : _*))
    case Schema.Value(_) => Json.obj("type" -> Json.fromString("string"))
    case Schema.Number(_, t) => Json.obj("type" -> Json.fromString("number"), "format" -> Json.fromString(t.format))
    case Schema.Integer(_, t) => Json.obj("type" -> Json.fromString("integer"), "format" -> Json.fromString(t.format))
    case Schema.Ref(id) => Json.obj("$ref" -> Json.fromString(s"#/definitions/$id"))

    case _ => Json.Null
  }

  implicit val encoderSchemaManifest = new Encoder[SchemaManifest] {
    override def apply(a: SchemaManifest): Json =
      Json.obj(a.manifest.toList.map { case (id, schema) => id -> schemaToJson(schema) } : _*)
  }
  
  implicit val encoderSwaggerType = new Encoder[SwaggerType] {
    override def apply(a: SwaggerType): Json = a match {
      case SwaggerType.String => Json.fromString("string") 
      case SwaggerType.Number => Json.fromString("number")
      case SwaggerType.Integer => Json.fromString("integer")
      case SwaggerType.Boolean => Json.fromString("boolean")
      case SwaggerType.File => Json.fromString("file")
    }
  }
  
  implicit val encoderSwaggerFormat = new Encoder[SwaggerFormat] {
    override def apply(a: SwaggerFormat): Json = a match {
      case SwaggerFormat.Int32 => Json.fromString("int32")
      case SwaggerFormat.Int64 => Json.fromString("int64")
      case SwaggerFormat.Float => Json.fromString("float")
      case SwaggerFormat.Double => Json.fromString("double")
      case SwaggerFormat.Byte => Json.fromString("byte")
      case SwaggerFormat.Binary => Json.fromString("binary")
      case SwaggerFormat.Boolean => Json.fromString("boolean")
      case SwaggerFormat.Date => Json.fromString("date")
      case SwaggerFormat.DateTime => Json.fromString("datetime")
      case SwaggerFormat.Password => Json.fromString("password")
    }
  }
  
  implicit val encoderSwaggerParameterIn = new Encoder[SwaggerParameter.In] {
    override def apply(a: SwaggerParameter.In): Json = a match {
      case SwaggerParameter.In.Path   => Json.fromString("path")
      case SwaggerParameter.In.Query  => Json.fromString("query")
      case SwaggerParameter.In.Header => Json.fromString("header")
      case SwaggerParameter.In.Form   => Json.fromString("form")
    }
  }

  implicit val encoderSwaggerParameter = new Encoder[SwaggerParameter] {

    def encoderGeneric(a: SwaggerParameter.Generic): Json = Json.obj(
      "name" -> Json.fromString(a.name),
      "in" -> encoderSwaggerParameterIn(a.in),
      "description" -> a.description.fold(Json.Null)(Json.fromString),
      "required" -> Json.fromBoolean(a.required),
      "type" -> encoderSwaggerType(a.`type`),
      "format" -> a.format.fold(Json.Null)(encoderSwaggerFormat.apply)
    )

    def encodeBody(a: SwaggerParameter.Body): Json = Json.obj(
      "name" -> Json.fromString(a.name),
      "in" -> Json.fromString("body"),
      "description" -> a.description.fold(Json.Null)(Json.fromString),
      "required" -> Json.fromBoolean(a.required),
      "schema" -> a.schema.fold(Json.Null)(schemaToJson)
    )

    override def apply(a: SwaggerParameter): Json = a match {
      case u: SwaggerParameter.Body => encodeBody(u)
      case u: SwaggerParameter.Generic => encoderGeneric(u)
    }
  }

  implicit val encoderSwaggerHeader = new Encoder[SwaggerHeader] {
    override def apply(a: SwaggerHeader): Json = Json.obj(
      "type" -> encoderSwaggerType(a.`type`),
      "description" -> a.description.fold(Json.Null)(Json.fromString),
      "format" -> a.format.fold(Json.Null)(encoderSwaggerFormat.apply)
    )
  }

  implicit val encoderSwaggerResponse = new Encoder[SwaggerResponse] {
    override def apply(a: SwaggerResponse): Json = Json.obj(
      "schema" -> a.schema.fold(Json.Null)(schemaToJson),
      "description" -> Json.fromString(a.description),
      "example" -> a.example.fold(Json.Null)(Json.fromString)
    )
  }

  implicit val encoderSwaggerOperation = new Encoder[SwaggerOperation] {
    override def apply(a: SwaggerOperation): Json = Json.obj(
      "produces" -> Json.arr(Json.fromString("application/json")),
      "parameters" -> Json.arr((a.path.parameters ++ a.parameters).map(encoderSwaggerParameter.apply) : _*),
      "description" -> a.description.fold(Json.Null)(Json.fromString),
      "responses" ->
        Json.obj(a.responses.byStatusCode.toList.map { case (code, response) => code.toString -> encoderSwaggerResponse(response) } : _*)
    )
  }

  implicit val encoderSwaggerApiInfo = new Encoder[SwaggerApiInfo] {
    override def apply(a: SwaggerApiInfo): Json = Json.obj(
      "title" -> Json.fromString(a.title),
      "version" -> Json.fromString(a.version),
      "description" -> Json.fromString(a.description),
    )
  }

  implicit val encoderSwaggerApi = new Encoder[SwaggerApi] {
    override def apply(a: SwaggerApi): Json = Json.obj(
      "swagger" -> Json.fromString("2.0"),
      "basePath" -> Json.fromString(a.basePath),
      "info" -> encoderSwaggerApiInfo(a.info),
      "definitions" -> encoderSchemaManifest(a.definitions),
      "paths" -> Json.obj(a.operations.groupBy(_.path.id).toList.map { case (path, operations) =>
        path -> Json.obj(operations.map(op => op.method.method -> encoderSwaggerOperation(op)): _*)
      } : _*)
    )
  }


}
