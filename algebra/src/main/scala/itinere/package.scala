

package object itinere {

  implicit class InvariantFunctorOps[F[_], A](val fa: F[A])(implicit I: InvariantFunctor[F]) {
    def imap[B](f: A => B)(g: B => A): F[B] = I.imap(fa)(f)(g)
    def as[B](implicit T: Transformer[F, A, B]): F[B] = T(fa)
  }

}
