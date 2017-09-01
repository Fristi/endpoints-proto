import shapeless._

package object itinere {

  implicit class InvariantFunctorOps[F[_], A](val fa: F[A])(implicit I: InvariantFunctor[F]) {
    def imap[B](f: A => B)(g: B => A): F[B] = I.imap(fa)(f)(g)
    def as[B](implicit T: Transformer[F, A, B]): F[B] = T(fa)
    def |[B <: Coproduct](b: F[B])(implicit C: CoCartesian[F]): F[A :+: B] = {
      I.imap(C.sum(fa, b)) {
        case Left(l) => Inl(l)
        case Right(r) => Inr(r)
      } {
        case Inl(l) => Left(l)
        case Inr(r) => Right(r)
      }
    }
  }

}
