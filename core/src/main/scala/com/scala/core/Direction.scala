package com.scala.core

import zio.json._

enum Direction {
    case Undirected
    case Forward
    case Backward

    def inverse: Direction = this match {
        case Undirected => Undirected
        case Forward => Backward
        case Backward => Forward
    }
}

type DirectedDirection = Direction.Forward.type | Direction.Backward.type

object Direction {
    given encoder: JsonEncoder[Direction] = DeriveJsonEncoder.gen[Direction]
    given decoder: JsonDecoder[Direction] = DeriveJsonDecoder.gen[Direction]
    
    given directedEncoder: JsonEncoder[DirectedDirection] = JsonEncoder[Direction].contramap(d => d: Direction)
    given directedDecoder: JsonDecoder[DirectedDirection] = JsonDecoder[Direction].mapOrFail({
        case d: DirectedDirection => Right(d)
        case other => Left(s"Invalid directed direction: $other")
    })
}