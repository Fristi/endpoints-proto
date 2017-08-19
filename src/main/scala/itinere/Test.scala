package itinere

import itinere.domain._



trait Test extends HttpEndpoints {


  def listUsers: Endpoint[ListUserRequest, ListUserResponse] = endpoint(
    request(Get, path / "test" /? optQs[String]("kind")).as[ListUserRequest],
    (
      response(400, entity = json[Error]).as[BadRequest] |
        (response(404, entity = json[Error]).as[NotFound] |
        (response(200, entity = json[List[User]]).as[Success] | cnil))
    ).as[ListUserResponse]
  )

}



object Main extends App {




}