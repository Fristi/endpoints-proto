package itinere

import itinere.domain._



trait Test extends HttpEndpointAlgebra {

  implicit def codec[A] = new JsonCodec[A] {
    override def encode(entity: A): String = ???

    override def decode(input: String): Either[String, A] = ???
  }


  def listUsers: Endpoint[ListUserRequest, ListUserResponse] = endpoint(
    request(GET, path / "test" /? optQs[String]("kind")).as[ListUserRequest],
    (
      response(400, entity = jsonResponse[Error]).as[BadRequest] |
        (response(404, entity = jsonResponse[Error]).as[NotFound] |
        (response(200, entity = jsonResponse[List[User]]).as[Success] | cnil))
    ).as[ListUserResponse]
  )

}



object Main extends App {




}