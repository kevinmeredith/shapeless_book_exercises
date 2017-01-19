package net

import shapeless.{::, Generic, HList, HNil}

object Penultimate {
  type Aux[L, O] = Penultimate[L] { type Out = O }

  def apply[H](implicit ev: Penultimate[H]): Aux[H, ev.Out] = ev

  implicit def secondToLast[H, G]: Aux[H :: G :: HNil, H] =
    new Penultimate[H :: G :: HNil] {
      type Out = H
      def apply(in: H :: G :: HNil): Out = in.head
    }

  implicit def inductivePenultimate[H, T <: HList, OutT](
     implicit penult: Aux[T, OutT]
  ): Aux[H :: T, OutT] = new Penultimate[H :: T] {
    type Out = OutT
    def apply(in: H :: T): Out = penult.apply(in.tail)
  }

  implicit def genericPenultimate[A, R, O](
   implicit gen: Generic.Aux[A, R],
   penultimate: Penultimate.Aux[R, O]
   ): Penultimate.Aux[A, O] = new Penultimate[A] {
      type Out = O
      def apply(in: A): Out = penultimate.apply(gen.to(in))
  }

  implicit class PenultimateOps[A](a: A) {
    def penultimate(implicit inst: Penultimate[A]): inst.Out = inst.apply(a)
  }
}

trait Penultimate[H] {
  type Out
  def apply(in: H): Out
}
