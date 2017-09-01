package itinere

import shapeless.ops.coproduct.Align
import shapeless.{Coproduct, Generic, HList}

trait Transformer[F[_], I, O] {
  def apply(fi: F[I]): F[O]
}

object Transformer {


  implicit def alignedCoproduct[F[_], I <: Coproduct, Repr <: Coproduct, O](implicit
                                                                         I: InvariantFunctor[F],
                                                                            G: Generic.Aux[O, Repr],
                                                                          TA: Align[Repr, I],
                                                                            FA: Align[I, Repr]): Transformer[F, I, O] = new Transformer[F, I, O] {
    override def apply(fi: F[I]): F[O] =
      I.imap(fi)(a => G.from(FA(a)))(b => TA(G.to(b)))
  }

  implicit def `from hlist to product`[F[_], I <: HList, O](implicit
                                                  I: InvariantFunctor[F],
                                                  F: Generic.Aux[O, I]): Transformer[F, I, O] = new Transformer[F, I, O] {
    override def apply(fi: F[I]): F[O] =
      I.imap(fi)(a => F.from(a))(b => F.to(b))
  }



}