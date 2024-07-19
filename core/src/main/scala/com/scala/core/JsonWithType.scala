package com.scala.core

import zio.json._
import zio.json.internal.{RetractReader, Write}

def EncoderWithType[T](typeName: String)(using encoder: JsonEncoder[T]): JsonEncoder[T] = new JsonEncoder[T] {
    case class Wrapper[T](@jsonField(typeName) val data: T) derives JsonEncoder
    override def unsafeEncode(a: T, indent: Option[Int], out: Write): Unit = {
        JsonEncoder[Wrapper[T]].unsafeEncode(Wrapper(a), indent, out)
  }
}

def DecoderWithType[T](typeName: String)(using decoder: JsonDecoder[T]): JsonDecoder[T] = new JsonDecoder[T] {
    case class Wrapper[T](@jsonField(typeName) val data: T) derives JsonDecoder
    override def unsafeDecode(trace: List[JsonError], in: RetractReader): T = {
        JsonDecoder[Wrapper[T]].unsafeDecode(trace, in).data
    }
}