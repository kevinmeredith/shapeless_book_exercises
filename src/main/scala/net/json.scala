package net

import shapeless.labelled.FieldType
import shapeless._

sealed trait JsonValue
case class JsonObject(fields: List[(String, JsonValue)]) extends JsonValue
case class JsonArray(items: List[JsonValue])             extends JsonValue
case class JsonString(value: String)                     extends JsonValue
case class JsonNumber(value: Double)                     extends JsonValue
case class JsonBoolean(value: Boolean)                   extends JsonValue
case object JsonNull                                     extends JsonValue

trait JsonEncoder[A] {
  def encode(value: A): JsonValue
}
object JsonEncoder {

  def apply[A](implicit ev: JsonEncoder[A]): JsonEncoder[A] =
    ev

  def instance[A](f: A => JsonValue): JsonEncoder[A] =
    new JsonEncoder[A] {
      override def encode(x: A): JsonValue = f(x)
    }

  implicit val doubleEncoder: JsonEncoder[Double] =
    instance[Double](JsonNumber)

  implicit val stringEncoder: JsonEncoder[String] =
    instance[String](JsonString)

  implicit val boolEncoder: JsonEncoder[Boolean] =
    instance[Boolean](JsonBoolean)

  implicit def optionEncoder[A](implicit ev: JsonEncoder[A]): JsonEncoder[Option[A]] =
    instance[Option[A]] {
      case Some(a) => ev.encode(a)
      case None    => JsonNull
    }

  implicit def listEncoder[A](implicit ev: JsonEncoder[A]): JsonEncoder[List[A]] =
    instance[List[A]]( list => JsonArray(list.map(ev.encode)))

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): JsonObject
  }

  def createObjectEncoder[A](fn: A => JsonObject): JsonObjectEncoder[A] =
    new JsonObjectEncoder[A] {
      override def encode(value: A): JsonObject = fn(value)
    }

  implicit val hnilEncoder: JsonObjectEncoder[HNil] =
    createObjectEncoder(hnil => JsonObject(Nil))

  implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](
    implicit
     witness: Witness.Aux[K],
     hEncoder: Lazy[JsonEncoder[H]],
     tEncoder: JsonObjectEncoder[T]
  ): JsonObjectEncoder[FieldType[K, H] :: T] =
    createObjectEncoder { hlist =>
      val fieldName = witness.value.name
      val head      = hEncoder.value.encode(hlist.head)
      val tail      = tEncoder.encode(hlist.tail)
      JsonObject( (fieldName, head) :: tail.fields )
  }

  implicit def genericObjectEncoder[A, H <: HList](
    implicit
    generic: LabelledGeneric.Aux[A, H],
    hEncoder: Lazy[JsonObjectEncoder[H]]
  ): JsonEncoder[A] =
    createObjectEncoder { value =>
      hEncoder.value.encode(  generic.to(value) )
    }

  implicit val cnilObjectEncoder: JsonObjectEncoder[CNil] =
    createObjectEncoder(cnil => throw new Exception("Inconceivable!"))

  implicit def coproductObjectEncoder[K <: Symbol, H, T <: Coproduct](
     implicit
     witness: Witness.Aux[K],
     hEncoder: Lazy[JsonEncoder[H]],
     tEncoder: JsonObjectEncoder[T]
   ): JsonObjectEncoder[FieldType[K, H] :+: T] = {
    val typeName = witness.value.name
    createObjectEncoder {
      case Inl(h) =>
        JsonObject(List(typeName -> hEncoder.value.encode(h)))

      case Inr(t) =>
        tEncoder.encode(t)
    }
  }

}