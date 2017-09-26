package itinere.swagger

import itinere.{CoCartesian, HttpResponseAlgebra, InvariantFunctor, Tupler}

trait SwaggerGenResponse extends HttpResponseAlgebra {
  self: SwaggerGen =>
  override type HttpResponseHeaders[A] = List[SwaggerParameter] => List[SwaggerParameter]
  override type HttpResponseEntity[A] = SwaggerResponse
  override type HttpResponse[A] = SwaggerResponses


  override def emptyResponseHeaders: (List[SwaggerParameter]) => List[SwaggerParameter] = headers => headers
  override def emptyResponse: SwaggerResponse = SwaggerResponse("empty response")
  override def cnil: SwaggerResponses = SwaggerResponses(Map.empty)

  override def response[A, B](statusCode: Int, headers: (List[SwaggerParameter]) => List[SwaggerParameter], entity: SwaggerResponse)(implicit T: Tupler[A, B]): SwaggerResponses =
    SwaggerResponses(Map(statusCode -> entity))

  override implicit val httpResponseCocartesian: CoCartesian[Lambda[A => SwaggerResponses]] = new CoCartesian[Lambda[A => SwaggerResponses]] {
    override def sum[A, B](fa: SwaggerResponses, fb: SwaggerResponses): SwaggerResponses = SwaggerResponses(fa.byStatusCode ++ fb.byStatusCode)
  }
  override implicit val httpResponseResponseHeadersInvariantFunctor: InvariantFunctor[Lambda[A => List[SwaggerParameter] => List[SwaggerParameter]]] =
    new InvariantFunctor[Lambda[A => List[SwaggerParameter] => List[SwaggerParameter]]] {
      override def imap[A, B](fa: (List[SwaggerParameter]) => List[SwaggerParameter])(f: (A) => B)(g: (B) => A): (List[SwaggerParameter]) => List[SwaggerParameter] = fa
    }
  override implicit val httpResponseEntityInvariantFunctor: InvariantFunctor[Lambda[A => SwaggerResponse]] = new InvariantFunctor[Lambda[A => SwaggerResponse]] {
    override def imap[A, B](fa: SwaggerResponse)(f: (A) => B)(g: (B) => A): SwaggerResponse = fa
  }
  override implicit val httpResponseInvariantFunctor: InvariantFunctor[Lambda[A => SwaggerResponses]] = new InvariantFunctor[Lambda[A => SwaggerResponses]] {
    override def imap[A, B](fa: SwaggerResponses)(f: (A) => B)(g: (B) => A): SwaggerResponses = fa
  }
}
