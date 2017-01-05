package net

import shapeless.{::, Generic, HList, HNil, Lazy}

object Foo {

  def apply[A](implicit ev: Foo[A]): Foo[A] = ev

  def instance[A](f: A => List[String]): Foo[A] =
    new Foo[A] {
      override def encode(x: A): List[String] = f(x)
    }

  implicit val hnilEncoder: Foo[HNil] =
    instance[HNil](hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit hEncoder: Foo[H],
    tEncoder: Foo[T]
  ): Foo[H :: T] =
    instance[H :: T] {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
    implicit gen: Generic[A] { type Repr = R },
    enc: Foo[R]): Foo[A] =
    instance[A] { a =>
      enc.encode( gen.to(a) )
    }

}

case class Bar(baz: Int, qux: String)
case class Bippy(bar: Bar)

trait Foo[A] {
  def encode(x: A): List[String]
}


