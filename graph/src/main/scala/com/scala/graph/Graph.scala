package com.scala.graph

trait  Graph[T] { self =>
    type Self[U] <: Graph[U]

    val vertices: Set[Vertex[_ <: T]] = Set()
    val edges: Set[Edge[_ <: T]] = Set()

    def addVertex[U <: T](v: Vertex[U]): Self[U] = {
        copy(newVertices = this.vertices + v)
    }

    def addEdge[U <: T](e: Edge[U]): Self[U] = {
        copy(newEdges = this.edges + e)
    }

    def removeVertex[U <: T](v: Vertex[U]): Self[U] = {
        copy(newVertices = this.vertices - v)
    }

    def removeEdge[U <: T](e: Edge[U]): Self[U] = {
        copy(newEdges = this.edges - e)
    }

    def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[Edge[_ <: T]] = Set()): Self[U]

    def copy[U <: T](newVertices: Set[Vertex[_ <: T]] = this.vertices, newEdges: Set[Edge[_ <: T]] = this.edges): Self[U] = {
        create(newVertices, newEdges)
    }

    def serialize: String = ""
    def deserialize[T](s: String): Graph[T] = ???

    override def toString(): String = {
        s"Graph:\nVertices:\n${this.vertices}\nEdges:\n${edges}"
    }
}

class DirectedGraph[T] extends Graph[T] {
    override type Self[U] <: DirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[Edge[_ <: T]] = Set()): Self[U] = {
        new DirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[Edge[U]] = newEdges.asInstanceOf[Set[Edge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[T](s: String): DirectedGraph[T] = {
        new DirectedGraph[T]
    }
}

class UndirectedGraph[T] extends Graph[T] {
    override type Self[U] <: UndirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[Edge[_ <: T]] = Set()): Self[U] = {
        new UndirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[Edge[U]] = newEdges.asInstanceOf[Set[Edge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[T](s: String): UndirectedGraph[T] = {
        new UndirectedGraph[T]
    }
}

class WeightedDirectedGraph[T] extends DirectedGraph[T] {
    override type Self[U] = WeightedDirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[Edge[_ <: T]] = Set()): Self[U] = {
        new WeightedDirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[Edge[U]] = newEdges.asInstanceOf[Set[Edge[U]]]
        }.asInstanceOf[Self[U]]
    }

    override def deserialize[T](s: String): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T]
    }
}

class WeightedUndirectedGraph[T] extends UndirectedGraph[T] {
    override type Self[U] = WeightedUndirectedGraph[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]] = Set(), newEdges: Set[Edge[_ <: T]] = Set()): Self[U] = {
        new WeightedUndirectedGraph[U] {
            override val vertices: Set[Vertex[U]] = newVertices.asInstanceOf[Set[Vertex[U]]]
            override val edges: Set[Edge[U]] = newEdges.asInstanceOf[Set[Edge[U]]]
        }.asInstanceOf[Self[U]]
    }
    override def deserialize[T](s: String): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T]
    }
}