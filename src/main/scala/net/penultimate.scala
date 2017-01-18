package net

import shapeless.{HList, HNil, ::}

object Penultimate {
  type Aux[L <: HList, O] = Penultimate[L] { type Out = O }

  implicit def secondToLast[H, G]: Aux[H :: G :: HNil, H] =
    new Penultimate[H :: G :: HNil] {
      override type Out = H
      override def apply(in: H :: G :: HNil): Out = in.head
    }

  implicit def inductive[H, T <: HList, OutT](
     implicit penult: Aux[T, OutT]
  ): Penultimate[H :: T] = new Penultimate[::[H, T]] {

    override type Out = OutT

    override def apply(in: H :: T): Out = penult.apply(in.tail)
  }
}

trait Penultimate[H <: HList] {
  type Out
  def apply(in: H): Out
}
