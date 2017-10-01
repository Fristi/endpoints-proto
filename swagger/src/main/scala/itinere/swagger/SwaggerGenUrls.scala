package itinere.swagger
import itinere.{InvariantFunctor, Tupler, UrlAlgebra}

trait SwaggerGenUrls extends UrlAlgebra {

  override type QueryString[A] = List[SwaggerParameter]
  override type QueryStringParam[A] = SwaggerType
  override type Segment[A] = SwaggerType
  override type Path[A] = SwaggerPath
  override type Url[A] = SwaggerPath

  override def combineQueryStrings[A, B](first: List[SwaggerParameter], second: List[SwaggerParameter])(implicit tupler: Tupler[A, B]): List[SwaggerParameter] = first ++ second

  override def qs[A](name: String, description: Option[String] = None)(implicit value: SwaggerType): List[SwaggerParameter] =
    List(SwaggerParameter.query(name = name, required = true, `type` = value, description = description))

  override implicit def stringQueryString: SwaggerType = SwaggerType.String
  override implicit def intQueryString: SwaggerType = SwaggerType.Integer
  override implicit def longQueryString: SwaggerType = SwaggerType.Integer

  override def optQs[A](name: String, description: Option[String] = None)(implicit value: SwaggerType): List[SwaggerParameter] =
    List(SwaggerParameter.query(name = name, required = false, `type` = value, description = description))

  override implicit def stringSegment: SwaggerType = SwaggerType.String

  override def staticPathSegment(segment: String): SwaggerPath = SwaggerPath(segment)

  override def segment[A](name: String, description: Option[String])(implicit s: SwaggerType): SwaggerPath = SwaggerPath(s"{$name}", List(SwaggerParameter.path(name, description, s)))

  override def chainPaths[A, B](first: SwaggerPath, second: SwaggerPath)(implicit tupler: Tupler[A, B]): SwaggerPath =
    SwaggerPath(s"${first.id}/${second.id}", first.parameters ++ second.parameters)

  override def urlWithQueryString[A, B](path: SwaggerPath, qs: List[SwaggerParameter])(implicit tupler: Tupler[A, B]): SwaggerPath = path.copy(parameters = path.parameters ++ qs)

  override implicit val segmentInvariantFunctor: InvariantFunctor[Lambda[A => SwaggerType]] = new InvariantFunctor[Lambda[A => SwaggerType]] {
    override def imap[A, B](fa: SwaggerType)(f: (A) => B)(g: (B) => A): SwaggerType = fa
  }
  override implicit val queryStringInvariantFunctor: InvariantFunctor[Lambda[A => List[SwaggerParameter]]] = new InvariantFunctor[Lambda[A => List[SwaggerParameter]]] {
    override def imap[A, B](fa: List[SwaggerParameter])(f: (A) => B)(g: (B) => A): List[SwaggerParameter] = fa
  }

//  override implicit val queryStringParamInvariantFunctor: InvariantFunctor[Lambda[A => Type]] = new InvariantFunctor[Lambda[A => Type]] {
//    override def imap[A, B](fa: Type)(f: (A) => B)(g: (B) => A): Type = fa
//  }
  override implicit val pathInvariantFunctor: InvariantFunctor[Lambda[A => SwaggerPath]] = new InvariantFunctor[Lambda[A => SwaggerPath]] {
    override def imap[A, B](fa: SwaggerPath)(f: (A) => B)(g: (B) => A): SwaggerPath = fa
  }
  override implicit val urlInvariantFunctor: InvariantFunctor[Lambda[A => SwaggerPath]] = new InvariantFunctor[Lambda[A => SwaggerPath]] {
    override def imap[A, B](fa: SwaggerPath)(f: (A) => B)(g: (B) => A): SwaggerPath = fa
  }
}
