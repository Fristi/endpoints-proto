package itinere.swagger

import cats.data.Writer
import cats.implicits._
import itinere.Schema


sealed trait SwaggerType

object SwaggerType {
  case object String extends SwaggerType
  case object Number extends SwaggerType
  case object Integer extends SwaggerType
  case object Boolean extends SwaggerType
  case object File extends SwaggerType
}
sealed trait SwaggerFormat

object SwaggerFormat {
  case object Int32 extends SwaggerFormat
  case object Int64 extends SwaggerFormat
  case object Float extends SwaggerFormat
  case object Double extends SwaggerFormat
  case object Byte extends SwaggerFormat
  case object Binary extends SwaggerFormat
  case object Boolean extends SwaggerFormat
  case object Date extends SwaggerFormat
  case object DateTime extends SwaggerFormat
  case object Password extends SwaggerFormat
}


sealed trait SwaggerParameter

object SwaggerParameter {
  sealed trait In

  object In {
    case object Path   extends In
    case object Query  extends In
    case object Header extends In
    case object Form   extends In
  }

  case class Body(description: Option[String] = None,
                  required: Boolean = false,
                  name: String = "body",
                  schema: Option[Schema] = None) extends SwaggerParameter

  case class Generic(name: String,
                     in: In,
                     required: Boolean = false,
                     description: Option[String] = None,
                     `type`: SwaggerType = SwaggerType.String,
                     format: Option[SwaggerFormat] = None) extends SwaggerParameter

  def query(name: String,
            required: Boolean = false,
            description: Option[String] = None,
            `type`: SwaggerType = SwaggerType.String,
            format: Option[SwaggerFormat] = None) =
    Generic(
      name = name,
      in = In.Query,
      required = required,
      description = description,
      `type` = `type`,
      format = format
    )

  def path(name: String,
           description: Option[String] = None,
           `type`: SwaggerType = SwaggerType.String,
           format: Option[SwaggerFormat] = None) =
    Generic(
      name = name,
      in = In.Path,
      required = true,
      description = description,
      `type` = `type`,
      format = format
    )
}

case class SwaggerHeader(`type`: SwaggerType,
                         format: Option[SwaggerFormat] = None,
                         description: Option[String] = None)



case class SwaggerOperation(method: SwaggerMethod,
                            path: SwaggerPath,
                            responses: SwaggerResponses = SwaggerResponses(),
                            parameters: List[SwaggerParameter] = Nil,
                            summary: Option[String] = None,
                            description: Option[String] = None) {
  def toReferenceTree: Writer[SchemaManifest, SwaggerOperation] = {
    def referenceSchemaBodyParam = parameters
      .collect { case u @ SwaggerParameter.Body(_,_,_, Some(schema)) => (u.name, u.description, u.required, schema) }
      .traverse[Writer[SchemaManifest, ?], SwaggerParameter.Body] { case (name, desc, req, s) =>
        SchemaTraverals.toReferenceTree(s).map(ss => SwaggerParameter.Body(desc, req, name, Some(ss)))
      }

    def referenceSchemaResponse = responses
      .byStatusCode
      .toList
      .collect { case (statusCode, SwaggerResponse(desc, headers, Some(schema), example)) => (statusCode, desc, headers, schema, example) }
      .traverse[Writer[SchemaManifest, ?], (Int, SwaggerResponse)] { case (statusCode, desc, headers, schema, example) =>
        SchemaTraverals.toReferenceTree(schema)
          .map(ss => statusCode -> SwaggerResponse(desc, headers, Some(ss), example))
      } map(x => SwaggerResponses(x.toMap))

    for {
      b <- referenceSchemaBodyParam
      r <- referenceSchemaResponse
    } yield copy(responses = r, parameters = b)
  }
}

case class SwaggerApiInfo(title: String, version: String, description: String)
case class SwaggerApi(info: SwaggerApiInfo, operations: List[SwaggerOperation], definitions: SchemaManifest, basePath: String)


case class SwaggerResponse(description: String,
                           headers: Map[String, SwaggerHeader] = Map.empty,
                           schema: Option[Schema] = None,
                           example: Option[String] = None)

case class SwaggerResponses(byStatusCode: Map[Int, SwaggerResponse] = Map.empty)

case class SwaggerPath(id: String,
                       parameters: List[SwaggerParameter] = Nil)

sealed abstract class SwaggerMethod(val method: String)
object SwaggerMethod {
  case object Get     extends SwaggerMethod("get")
  case object Delete  extends SwaggerMethod("delete")
  case object Post    extends SwaggerMethod("post")
  case object Put     extends SwaggerMethod("put")
  case object Patch   extends SwaggerMethod("patch")
  case object Head    extends SwaggerMethod("head")
  case object Options extends SwaggerMethod("options")
}


