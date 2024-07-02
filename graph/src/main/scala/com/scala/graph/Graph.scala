package com.scala.graph

trait  GraphLike[T, E <: EdgeLike[_ <: T]] { self =>
    type Self[U]

    val vertices: Set[Vertex[_ <: T]] = Set()
    val edges: Set[E] = Set()

    def addVertex[U <: T](v: Vertex[U]): Self[U] = {
        copy(newVertices = this.vertices + v)
    }

    def addEdge[U <: T](e: E): Self[U] = {
        copy(newEdges = this.edges + e)
    }

    def removeVertex[U <: T](v: Vertex[U]): Self[U] = {
        copy(newVertices = this.vertices - v)
    }

    def removeEdge[U <: T](e: E): Self[U] = {
        copy(newEdges = this.edges - e)
    }

    def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[E] = Set()): Self[U]

    def copy[U <: T](newVertices: Set[Vertex[_ <: T]] = this.vertices, newEdges: Set[E] = this.edges): Self[U] = {
        create(newVertices, newEdges)
    }

    def serialize: String = ""
    def deserialize[U <: T](s: String): Self[U] = ???

    override def toString(): String = {
        s"Graph:\nVertices:\n${this.vertices}\nEdges:\n${edges}"
    }
}

trait DirectedGraphLike[T, E <: DirectedEdgeLike[T]] extends GraphLike[T, E] {
}

trait UndirectedGraphLike[T, E <: UndirectedEdgeLike[T]] extends GraphLike[T, E] {
}

class DirectedGraph[T] extends DirectedGraphLike[T, DirectedEdge[T]] {
    override type Self[U] = DirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[DirectedEdge[T]] = Set()): Self[U] = {
        new DirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[DirectedEdge[U]] = newEdges.asInstanceOf[Set[DirectedEdge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        create()
    }
}

class UndirectedGraph[T] extends UndirectedGraphLike[T, UndirectedEdge[T]] {
    override type Self[U] = UndirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[UndirectedEdge[T]] = Set()): Self[U] = {
        new UndirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[UndirectedEdge[U]] = newEdges.asInstanceOf[Set[UndirectedEdge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        create()
    }
}

class WeightedDirectedGraph[T] extends DirectedGraphLike[T, WeightedDirectedEdge[T]] {
    override type Self[U] = WeightedDirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[WeightedDirectedEdge[T]] = Set()): Self[U] = {
        new WeightedDirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[WeightedDirectedEdge[U]] = newEdges.asInstanceOf[Set[WeightedDirectedEdge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        create()
    }
}

class WeightedUndirectedGraph[T] extends UndirectedGraphLike[T, WeightedUndirectedEdge[T]] {
    override type Self[U] = WeightedUndirectedGraph[U]
    
    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[WeightedUndirectedEdge[T]] = Set()): Self[U] = {
        new WeightedUndirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[WeightedUndirectedEdge[U]] = newEdges.asInstanceOf[Set[WeightedUndirectedEdge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        create()
    }
}