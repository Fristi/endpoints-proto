package itinere.swagger

import itinere.{HttpRequestAlgebra, InvariantFunctor, Tupler}

trait SwaggerGenRequest extends HttpRequestAlgebra with SwaggerGenUrls {
  self: SwaggerGen =>
  override type HttpRequestHeaders[A] = List[SwaggerParameter] => List[SwaggerParameter]
  override type HttpRequestEntity[A] = Option[SwaggerParameter.Body]
  override type HttpRequest[A] = SwaggerOperation
  override type HttpMethod = SwaggerMethod

  override def emptyRequestHeaders: (List[SwaggerParameter]) => List[SwaggerParameter] = headers => headers
  override def emptyRequestEntity: Option[SwaggerParameter.Body] = None

  override def GET: SwaggerMethod = SwaggerMethod.Get
  override def PUT: SwaggerMethod = SwaggerMethod.Put
  override def POST: SwaggerMethod = SwaggerMethod.Post
  override def DELETE: SwaggerMethod = SwaggerMethod.Delete
  override def PATCH: SwaggerMethod = SwaggerMethod.Patch

  override def request[A, B, C, AB](
    method: SwaggerMethod,
    url: SwaggerPath,
    headers: (List[SwaggerParameter]) => List[SwaggerParameter],
    entity: Option[SwaggerParameter.Body])(implicit T: Tupler.Aux[A, B, AB], TO: Tupler[AB, C]): SwaggerOperation =
    SwaggerOperation(method, url, parameters = entity.toList)

  override implicit val httpRequestHeadersInvariantFunctor: InvariantFunctor[Lambda[A => List[SwaggerParameter] => List[SwaggerParameter]]] = new InvariantFunctor[Lambda[A => List[SwaggerParameter] => List[SwaggerParameter]]] {
    override def imap[A, B](fa: (List[SwaggerParameter]) => List[SwaggerParameter])(f: (A) => B)(g: (B) => A): (List[SwaggerParameter]) => List[SwaggerParameter] = headers => fa(headers)
  }
  override implicit val httpRequestEntityInvariantFunctor: InvariantFunctor[Lambda[A => Option[SwaggerParameter.Body]]] = new InvariantFunctor[Lambda[A => Option[SwaggerParameter.Body]]] {
    override def imap[A, B](fa: Option[SwaggerParameter.Body])(f: (A) => B)(g: (B) => A): Option[SwaggerParameter.Body] = fa
  }
  override implicit val httpRequestInvariantFunctor: InvariantFunctor[Lambda[A => SwaggerOperation]] = new InvariantFunctor[Lambda[A => SwaggerOperation]] {
    override def imap[A, B](fa: SwaggerOperation)(f: (A) => B)(g: (B) => A): SwaggerOperation = fa
  }

}
