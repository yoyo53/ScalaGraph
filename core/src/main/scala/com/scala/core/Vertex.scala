package com.scala.core

import zio.json._
import zio.json.internal.{RetractReader, Write}

case class Vertex[+T](val id: Int, val data: Option[T]) {
}

object Vertex {
    def apply[T](id: Int, data: T): Vertex[T] = new Vertex[T](id, Some(data))

    def apply[T](id: Int): Vertex[T] = new Vertex[T](id, None)

    implicit def noneEncoder: JsonEncoder[None.type] = new JsonEncoder[None.type] {
        override def unsafeEncode(a: None.type, indent: Option[Int], out: Write): Unit = out.write("null")
    }
    implicit def noneDecoder: JsonDecoder[None.type] = DeriveJsonDecoder.gen[None.type]

    implicit def optionEncoder[T: JsonEncoder]: JsonEncoder[Option[T]] = new JsonEncoder[Option[T]] {
        override def unsafeEncode(a: Option[T], indent: Option[Int], out: Write): Unit = a match {
            case Some(value) => implicitly[JsonEncoder[T]].unsafeEncode(value, indent, out)
            case None        => JsonEncoder[None.type].unsafeEncode(None, indent, out)
        }
    }
    implicit def optionDecoder[T: JsonDecoder]: JsonDecoder[Option[T]] = new JsonDecoder[Option[T]] {    
        override def unsafeDecode(trace: List[JsonError], in: RetractReader): Option[T] = {
            in.nextNonWhitespace() match {
                case 'n' => {
                    in.retract()
                    JsonDecoder[None.type].unsafeDecode(trace, in)
                }
                case _ => {
                    in.retract()
                    Some(implicitly[JsonDecoder[T]].unsafeDecode(trace, in))
                }
            }
        }
    }

    implicit def encoder[T: JsonEncoder]: JsonEncoder[Vertex[T]] = DeriveJsonEncoder.gen[Vertex[T]]
    implicit def decoder[T: JsonDecoder]: JsonDecoder[Vertex[T]] = DeriveJsonDecoder.gen[Vertex[T]]
}