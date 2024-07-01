
case class Vertex(val graph: Graph, val name: String) {
    override def toString: String = name

    override def equals(obj: Any): Boolean = obj match {
        case v: Vertex => name == v.name
        case _ => false
    }

    override def hashCode: Int = {
        val prime = 31
        var result = 1
        result = prime * result + name.hashCode
        result
    }
}
