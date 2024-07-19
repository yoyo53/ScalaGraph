package com.scala.core

import zio.json._
import zio.json.internal.{RetractReader, Write}

case class Vertex[+T](val id: Int, val data: Option[T]) {
}

object Vertex {
    def apply[T](id: Int, data: T): Vertex[T] = new Vertex[T](id, Some(data))

    def apply[T](id: Int): Vertex[T] = new Vertex[T](id, None)

    given encoder[T: JsonEncoder]: JsonEncoder[Vertex[T]] = DeriveJsonEncoder.gen[Vertex[T]]
    given decoder[T: JsonDecoder]: JsonDecoder[Vertex[T]] = DeriveJsonDecoder.gen[Vertex[T]]
}