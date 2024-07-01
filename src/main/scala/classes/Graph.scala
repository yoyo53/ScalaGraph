class Graph {
    private val vertices = scala.collection.mutable.Set[Vertex]()
    private val edges = scala.collection.mutable.Set[Edge]()

    override def toString: String = {
        val v = vertices.map(_.toString).mkString(", ")
        val e = edges.map(_.toString).mkString(", ")
        s"Vertices: $v\nEdges: $e"
    }
}