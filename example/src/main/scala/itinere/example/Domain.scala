package itinere.example

case class Error(message: String)
case class User(name: String, age: Int)

case class ListUserRequest(kind: Option[String])
case class AddUserRequest(contentType: String, ipAddress: String, age: Int, user: User)

sealed trait DomainResponse[+A]
final case class Success[A](users: A) extends DomainResponse[A]
final case class BadRequest(error: Error) extends DomainResponse[Nothing]
final case class NotFound(error: Error) extends DomainResponse[Nothing]
