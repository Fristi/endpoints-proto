package itinere.swagger

import itinere.{HttpJsonAlgebra, JsonCodec, JsonSchema, WithJsonCodec}


trait SwaggerGenJson extends HttpJsonAlgebra { self: SwaggerGen with WithJsonCodec =>
  override def jsonResponse[A: JsonCodec : JsonSchema](description: Option[String] = None): SwaggerResponse =
    SwaggerResponse(description = description, headers = Map.empty, schema = Some(implicitly[JsonSchema[A]].schema))

  override def jsonRequest[A: JsonCodec : JsonSchema](description: Option[String] = None): Option[SwaggerParameter.Body] =
    Some(SwaggerParameter.Body(description = description, required = true, name = "body", schema = Some(implicitly[JsonSchema[A]].schema)))
}
