package itinere.domain

case class Error(message: String)
case class User(name: String)

case class ListUserRequest(kind: Option[String])

sealed trait ListUserResponse
final case class Success(users: List[User]) extends ListUserResponse
final case class BadRequest(error: Error) extends ListUserResponse
final case class NotFound(error: Error) extends ListUserResponse
