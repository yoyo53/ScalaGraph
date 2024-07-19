package com.scala.core

import zio.json._

sealed trait EdgeLike[+T] {
    val v1: Vertex[T]
    val v2: Vertex[T]
    val direction: Direction
    val weight: Option[Int]

    override def equals(obj: Any): Boolean = obj match {
        case e: EdgeLike[_] => weight == e.weight && (
            (v1 == e.v1 && v2 == e.v2 && direction == e.direction)
            || (v1 == e.v2 && v2 == e.v1 && direction == e.direction.inverse)
        )
        case _ => false
    }

    override def hashCode: Int = {
        val prime = 31
        val result = direction match {
            case Direction.Undirected => prime * prime * v1.hashCode + v2.hashCode
            case Direction.Forward => prime * v2.hashCode + v1.hashCode
            case Direction.Backward => prime * v1.hashCode + v2.hashCode
        }
        result * prime + weight.hashCode
    }
}

object EdgeLike {
}



sealed trait DirectedEdgeLike[+T] extends EdgeLike[T] {
    val direction: DirectedDirection
}

sealed trait UndirectedEdgeLike[+T] extends EdgeLike[T] {
    val direction = Direction.Undirected
}

sealed trait WeightedEdgeLike[+T] extends EdgeLike[T] {
    val weight: Some[Int]
}

sealed trait UnweightedEdgeLike[+T] extends EdgeLike[T] {
    val weight = None
}



case class DirectedEdge[+T](val v1: Vertex[T], val v2: Vertex[T], val direction: DirectedDirection) extends DirectedEdgeLike[T] with UnweightedEdgeLike[T] {
}

object DirectedEdge {
    given encoder[T: JsonEncoder]: JsonEncoder[DirectedEdge[T]] = DeriveJsonEncoder.gen[DirectedEdge[T]]
    given decoder[T: JsonDecoder]: JsonDecoder[DirectedEdge[T]] = DeriveJsonDecoder.gen[DirectedEdge[T]]
}



case class UndirectedEdge[+T](val v1: Vertex[T], val v2: Vertex[T]) extends UndirectedEdgeLike[T] with UnweightedEdgeLike[T] {
}

object UndirectedEdge {
    given encoder[T: JsonEncoder]: JsonEncoder[UndirectedEdge[T]] = DeriveJsonEncoder.gen[UndirectedEdge[T]]
    given decoder[T: JsonDecoder]: JsonDecoder[UndirectedEdge[T]] = DeriveJsonDecoder.gen[UndirectedEdge[T]]
}



case class WeightedDirectedEdge[+T](val v1: Vertex[T], val v2: Vertex[T], direction: DirectedDirection, val weight: Some[Int]) extends DirectedEdgeLike[T] with WeightedEdgeLike[T] {
}

object WeightedDirectedEdge {
    def apply[T](v1: Vertex[T], v2: Vertex[T], direction: DirectedDirection, weight: Int): WeightedDirectedEdge[T] = {
        new WeightedDirectedEdge(v1, v2, direction, Some(weight))
    }

    given someIntEncoder: JsonEncoder[Some[Int]] = JsonEncoder[Int].contramap(_.get)
    given someIntDecoder: JsonDecoder[Some[Int]] = JsonDecoder[Int].map(Some(_))

    given encoder[T: JsonEncoder]: JsonEncoder[WeightedDirectedEdge[T]] = DeriveJsonEncoder.gen[WeightedDirectedEdge[T]]
    given decoder[T: JsonDecoder]: JsonDecoder[WeightedDirectedEdge[T]] = DeriveJsonDecoder.gen[WeightedDirectedEdge[T]]
}



case class WeightedUndirectedEdge[+T](val v1: Vertex[T], val v2: Vertex[T], val weight: Some[Int]) extends UndirectedEdgeLike[T] with WeightedEdgeLike[T] {
}

object WeightedUndirectedEdge {
    def apply[T](v1: Vertex[T], v2: Vertex[T], weight: Int): WeightedUndirectedEdge[T] = {
        new WeightedUndirectedEdge(v1, v2, Some(weight))
    }

    given someIntEncoder: JsonEncoder[Some[Int]] = JsonEncoder[Int].contramap(_.get)
    given someIntDecoder: JsonDecoder[Some[Int]] = JsonDecoder[Int].map(Some(_))

    given encoder[T: JsonEncoder]: JsonEncoder[WeightedUndirectedEdge[T]] = DeriveJsonEncoder.gen[WeightedUndirectedEdge[T]]
    given decoder[T: JsonDecoder]: JsonDecoder[WeightedUndirectedEdge[T]] = DeriveJsonDecoder.gen[WeightedUndirectedEdge[T]]
}