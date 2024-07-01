trait Graph {
    def vertices: Set[Vertex] = scala.collection.immutable.Set()
    def edges: Set[Edge] = scala.collection.immutable.Set()

    override def toString: String = {
        val v = vertices.map(_.toString).mkString(", ")
        val e = edges.map(_.toString).mkString(", ")
        s"Vertices:\n$v\nEdges:\n$e"
    }
}

class DirectedGraph extends Graph {
}

class UndirectedGraph extends Graph {
}

class WeightedDirectedGraph extends DirectedGraph {
}

class WeightedUndirectedGraph extends UndirectedGraph {
}