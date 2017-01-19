package net

import shapeless.{::, DepFn1, HList}

trait Init[L <: HList] extends DepFn1[L]

object Init {

  type Aux[L <: HList, Out1] = Init[L] { type Out = Out1}

  def apply[L <: HList](implicit ev: Init[L]): Aux[L, ev.Out] = ev

  implicit def atLeast1[H, L <: HList]: Aux[H :: L, L] =
    new Init[H :: L] {
      type Out = L
      def apply(in: H :: L): Out = in.tail
    }
}