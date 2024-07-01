package com.scala.graph

case class Edge[+T](val v1: Vertex[T], val v2: Vertex[T], val direction: Direction = Direction.Undirected, val weight: Option[Int] = None) {
    override def toString: String = {
        val weightStr = if (weight.isDefined) s" (${weight.get})" else ""
        val dir = direction match {
            case Direction.Undirected => "--"
            case Direction.Forward => "->"
            case Direction.Backward => "<-"
        }
        s"${v1.toString} ${dir} ${v2.toString}${weightStr}"
    }

    override def equals(obj: Any): Boolean = obj match {
        case e: Edge[T] => weight == e.weight && (
            (v1 == e.v1 && v2 == e.v2 && direction == e.direction)
            || (v1 == e.v2 && v2 == e.v1 && direction == e.direction.inverse)
        )
        case _ => false
    }

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + v1.hashCode
        result = prime * result + v2.hashCode
        result = prime * result + direction.hashCode
        result
    }
}