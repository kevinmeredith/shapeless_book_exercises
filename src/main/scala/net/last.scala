package net

import shapeless.{HList, HNil, ::}

object Last {
  type Aux[L <: HList, O] = Last[L] { type Out = O }

  implicit def singleLast[H]: Aux[H :: HNil, H] = new Last[H :: HNil] {
    override type Out = H
    override def last(in: H :: HNil): H = in.head
  }

  implicit def hlistLast[H, T <: HList, OutT]
  (implicit lt : Last.Aux[T, OutT]): Aux[H :: T, OutT] =
    new Last[H :: T] {
      type Out = OutT
      def last(l : H :: T): Out = lt.last(l.tail)
    }
}

trait Last[H <: HList] {
  type Out
  def last(in: H): Out
}
