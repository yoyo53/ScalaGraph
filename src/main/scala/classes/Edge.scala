class Edge(val graph: Graph, val src: Vertex, val dst: Vertex, val weight: Int) {
    override def toString: String = s"${src.toString} -> ${dst.toString} ($weight)"

    override def equals(obj: Any): Boolean = obj match {
        case e: Edge => src == e.src && dst == e.dst && weight == e.weight
        case _ => false
    }

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + src.hashCode
        result = prime * result + dst.hashCode
        result = prime * result + weight
        result
    }
}