package itinere.domain

case class Error(message: String)
case class User(name: String)

case class ListUserRequest(kind: Option[String])

sealed trait DomainResponse[+A]
final case class Success[A](users: A) extends DomainResponse[A]
final case class BadRequest(error: Error) extends DomainResponse[Nothing]
final case class NotFound(error: Error) extends DomainResponse[Nothing]
