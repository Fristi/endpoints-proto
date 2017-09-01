package itinere

import akka.http.scaladsl.server._
import shapeless.HNil

package object server {
  def joinDirectives[T1, T2](dir1: Directive1[T1], dir2: Directive1[T2])(implicit tupler: Tupler[T1, T2]): Directive1[tupler.Out] = {
    Directive[Tuple1[tupler.Out]] { inner =>
      dir1.tapply { case Tuple1(prefix) => dir2.tapply { case Tuple1(suffix) => inner(Tuple1(tupler(prefix, suffix))) } }
    }
  }

  def convToDirective1(directive: Directive0): Directive1[HNil] =
    directive.tflatMap[Tuple1[HNil]](_ => Directive.apply[Tuple1[HNil]](f => f(Tuple1(HNil))))
}
