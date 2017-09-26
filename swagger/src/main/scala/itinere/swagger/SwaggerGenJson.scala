package itinere.swagger

import itinere.{HttpJsonAlgebra, JsonCodec, JsonSchema, WithJsonCodec}


trait SwaggerGenJson extends HttpJsonAlgebra { self: SwaggerGen with WithJsonCodec =>
  override def jsonResponse[A: JsonCodec : JsonSchema]: SwaggerResponse =
    SwaggerResponse("", Map.empty, Some(implicitly[JsonSchema[A]].schema))

  override def jsonRequest[A: JsonCodec : JsonSchema]: Option[SwaggerParameter.Body] =
    Some(SwaggerParameter.Body(None, false, "body", Some(implicitly[JsonSchema[A]].schema)))
}
