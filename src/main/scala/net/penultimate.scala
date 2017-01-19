package net

import shapeless.{HList, HNil, ::}

object Penultimate {
  type Aux[L <: HList, O] = Penultimate[L] { type Out = O }

  def apply[H <: HList](implicit ev: Penultimate[H]): Aux[H, ev.Out] = ev

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
}

trait Penultimate[H <: HList] {
  type Out
  def apply(in: H): Out
}
