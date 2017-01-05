package net

import shapeless._

case class Person(name: String, age: Int, isManager: Boolean)
case class IceCream(style: String, jimmies: Int, hasHotFudge: Boolean)

object CsvEncoder {

  def apply[A](implicit ev: CsvEncoder[A]): CsvEncoder[A] = ev

  def instance[A](f: A => List[String]): CsvEncoder[A] =
    new CsvEncoder[A] {
      override def encode(x: A): List[String] = f(x)
    }

  implicit val boolEncoder: CsvEncoder[Boolean] =
    instance[Boolean](b => if(b) List("yes") else List("no") )

  implicit val intEncoder: CsvEncoder[Int] =
    instance[Int](i => List(i.toString))

  implicit val strEncoder: CsvEncoder[String] =
    instance[String](List(_))

  implicit val doubleEncoder: CsvEncoder[Double] =
    instance[Double](d => List(d.toString))

  implicit val hnilEncoder: CsvEncoder[HNil] =
    instance[HNil](hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit hEncoder: Lazy[CsvEncoder[H]],
             tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    instance[H :: T] {
      case h :: t => hEncoder.value.encode(h) ++ tEncoder.encode(t)
    }

  implicit def genericEncoder[A, R](
    implicit gen: Generic[A] { type Repr = R },
    enc: Lazy[CsvEncoder[R]]): CsvEncoder[A] =
    instance[A] { a =>
      enc.value.encode( gen.to(a) )
    }

  implicit val cnilEncoder: CsvEncoder[CNil] =
    instance(cnil => throw new RuntimeException("Inconceivable!"))

  implicit def coproductEncoder[H, T <: Coproduct](
    implicit
      hEncoder: Lazy[CsvEncoder[H]],
      tEncoder: CsvEncoder[T]
    ): CsvEncoder[H :+: T] = instance {
    case Inl(h) => hEncoder.value.encode(h)
    case Inr(t) => tEncoder.encode(t)
  }

}

sealed trait Shape
final case class Rectangle(width: Double, height: Double) extends Shape
final case class Circle(radius: Double)                   extends Shape


trait CsvEncoder[A] {
  def encode(x: A): List[String]
}

sealed trait Tree[+A]
case object Empty                                     extends Tree[Nothing]
case class Node[+A](l: Tree[A], value: A, r: Tree[A]) extends Tree[A]