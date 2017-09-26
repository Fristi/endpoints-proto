package itinere.swagger

import itinere.HttpEndpointAlgebra

class SwaggerGen extends HttpEndpointAlgebra
  with SwaggerGenUrls
  with SwaggerGenRequest
  with SwaggerGenResponse
{
  override type Endpoint[A, B] = SwaggerOperation

  override def endpoint[A, B](request: SwaggerOperation, response: SwaggerResponses): SwaggerOperation =
    request.copy(responses = response)
}