

import itinere.domain._
import shapeless.{:+:, CNil, Generic}


val t =
  implicitly[Generic.Aux[ListUserResponse, BadRequest :+: NotFound :+: Success :+: CNil]]

