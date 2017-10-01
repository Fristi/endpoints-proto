package itinere.swagger

import cats.data.Writer
import itinere.HttpEndpointAlgebra
import cats.implicits._



class SwaggerGen extends HttpEndpointAlgebra
  with SwaggerGenUrls
  with SwaggerGenRequest
  with SwaggerGenResponse
{
  override type Endpoint[A, B] = SwaggerOperation

  override def endpoint[A, B](request: SwaggerOperation, response: SwaggerResponses, description: Option[String] = None): SwaggerOperation =
    request.copy(responses = response, description = description)

  def api(info: SwaggerApiInfo, basePath: String)(ops: SwaggerOperation*): SwaggerApi = {
    val (manifest, operations) = ops.toList.traverse[Writer[SchemaManifest, ?], SwaggerOperation](_.toReferenceTree).run

    SwaggerApi(info, operations, manifest, basePath)
  }
}