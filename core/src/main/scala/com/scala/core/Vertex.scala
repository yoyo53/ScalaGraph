package com.scala.core

import zio.json._

case class Vertex[+T](val id: Int, val data: Option[T]) {
}

object Vertex {
  def apply[T](id: Int, data: T): Vertex[T] = new Vertex(id, Some(data))

  def apply[T](id: Int): Vertex[T] = new Vertex(id, None)

  implicit def optionTEncoder[T]: JsonEncoder[Option[T]] = JsonEncoder[Option[T]] {(a, indent, out) =>
      a match {
        case Some(value) if value.isInstanceOf[Number | Boolean] => out.write(s"${value}")
        case Some(value) => out.write(s"\"${value.toString}\"")
        case None        => out.write("null")
      }
    }
  implicit def encoder[T]: JsonEncoder[Vertex[T]] = DeriveJsonEncoder.gen[Vertex[T]]

  implicit def optionTDecoder[T]: JsonDecoder[Option[T]] = DeriveJsonDecoder.gen[None.type].map(_ => None)
  implicit def decoder[T]: JsonDecoder[Vertex[T]] = DeriveJsonDecoder.gen[Vertex[T]]
}