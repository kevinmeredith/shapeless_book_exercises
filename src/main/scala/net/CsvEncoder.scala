package net

import shapeless._

case class Person(name: String, age: Int, isManager: Boolean)

object CsvEncoder {

  def apply[A](x: A)(implicit ev: CsvEncoder[A]): CsvEncoder[A] = ev

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

  implicit val hnilEncoder: CsvEncoder[HNil] =
    instance[HNil](hnil => Nil)

  implicit def hlistEncoder[H, T <: HList](
    implicit hEncoder: CsvEncoder[H],
             tEncoder: CsvEncoder[T]
  ): CsvEncoder[H :: T] =
    instance[H :: T] {
      case h :: t => hEncoder.encode(h) ++ tEncoder.encode(t)
    }

  implicit val personEncoderInternal: CsvEncoder[String :: Int :: Boolean :: HNil] =
    implicitly

  implicit val personEncoder: CsvEncoder[Person] = {
    val gen = Generic[Person]
    instance[Person] { p =>
      personEncoderInternal.encode( gen.to(p) )
    }
  }

}

trait CsvEncoder[A] {
  def encode(x: A): List[String]
}