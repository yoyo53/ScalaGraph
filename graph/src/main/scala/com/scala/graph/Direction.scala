package com.scala.graph

enum Direction {
    case Undirected, Forward, Backward
    def inverse: Direction = this match {
        case Undirected => Undirected
        case Forward => Backward
        case Backward => Forward
    }
}

type DirectedDirection = Direction.Forward.type | Direction.Backward.type
