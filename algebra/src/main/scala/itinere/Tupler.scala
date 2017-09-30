package itinere

import shapeless._
import shapeless.ops.hlist.{Prepend, Split}

trait Tupler[A, B] {
  type Out <: HList
  def apply(a: A, b: B): Out
  def unapply(out: Out): (A, B)
}

object Tupler extends Tupler5

trait Tupler0 {
  type Aux[A, B, Out0] = Tupler[A, B] { type Out = Out0 }

  implicit def ab[A, B]: Aux[A, B, A :: B :: HNil] = new Tupler[A, B] {
    type Out = A :: B :: HNil
    def apply(a: A, b: B): Out = a :: b :: HNil
    override def unapply(out: ::[A, ::[B, HNil]]): (A, B) = out.head -> out.tail.head
  }

}

trait Tupler1 extends Tupler0 {

  implicit def left[A, B <: HList]: Aux[A, B, A :: B] =
    new Tupler[A, B] {
      type Out = A :: B
      def apply(a: A, bc: B): A :: B = a :: bc
      override def unapply(out: ::[A, B]): (A, B) = out.head -> out.tail
    }

  implicit def right[A <: HList, B, C <: HList, N <: Nat](implicit P: Prepend.Aux[A, B :: HNil, C], S: Split.Aux[C, N, A, B :: HNil]): Aux[A, B, C] =
    new Tupler[A, B] {
      type Out = C
      def apply(a: A, b: B): P.Out = P.apply(a, b :: HNil)
      override def unapply(out: C): (A, B) = {
        val s = S(out)

        s._1 -> s._2.head
      }
    }

}

trait Tupler2 extends Tupler1 {
  implicit def leftRight[A <: HList, B <: HList](implicit prepend : Prepend[A, B]): Aux[A, B, prepend.Out] =
    new Tupler[A, B] {
      type Out = prepend.Out
      def apply(a: A, b: B) = prepend.apply(a, b)
      override def unapply(out: prepend.Out): (A, B) = ???
    }
}

trait Tupler3 extends Tupler2 {

  implicit def leftUnit[A]: Aux[HNil, A, A :: HNil] = new Tupler[HNil, A] {
    type Out = A :: HNil
    def apply(a: HNil, b: A): A :: HNil = b :: HNil
    override def unapply(out: ::[A, HNil]): (HNil, A) = HNil -> out.head
  }

}

trait Tupler4 extends Tupler3 {

  implicit def rightUnit[A]: Aux[A, HNil, A :: HNil] = new Tupler[A, HNil] {
    type Out = A :: HNil
    def apply(a: A, b: HNil): A :: HNil = a :: HNil

    override def unapply(out: ::[A, HNil]): (A, HNil) = out.head -> HNil
  }

}

trait Tupler5 extends Tupler4 {

  implicit def leftUnitRightHlist[B <: HList]: Aux[HNil, B, B] = new Tupler[HNil, B] {
    override type Out = B
    override def apply(a: HNil, b: B): B = b

    override def unapply(out: B): (HNil, B) = HNil -> out
  }

  implicit def leftHListRightUnit[A <: HList]: Aux[A, HNil, A] = new Tupler[A, HNil] {
    override type Out = A
    override def apply(a: A, b: HNil): A = a

    override def unapply(out: A): (A, HNil) = out -> HNil
  }

  implicit def leftAndRightUnit: Aux[HNil, HNil, HNil] = new Tupler[HNil, HNil] {
    override type Out = HNil
    override def apply(a: HNil, b: HNil): HNil = HNil

    override def unapply(out: HNil): (HNil, HNil) = HNil -> HNil
  }
}

