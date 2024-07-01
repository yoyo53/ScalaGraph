sealed trait Direction {
    def inverse: Direction
}

object Direction {
    case object Undirected extends Direction {
        def inverse: Direction = Undirected
    }
    case object Forward extends Direction {
        def inverse: Direction = Backward
    }
    case object Backward extends Direction {
        def inverse: Direction = Forward
    }
}