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
    implicit def encoder: JsonEncoder[Direction] = DeriveJsonEncoder.gen[Direction]
    implicit def decoder: JsonDecoder[Direction] = DeriveJsonDecoder.gen[Direction]
    
    implicit def directedEncoder: JsonEncoder[DirectedDirection] = JsonEncoder[Direction].contramap(d => d: Direction)
    implicit def directedDecoder: JsonDecoder[DirectedDirection] = JsonDecoder[Direction].map({
        case d: DirectedDirection => d
        case _ => throw new Exception("Invalid direction")
    })
}