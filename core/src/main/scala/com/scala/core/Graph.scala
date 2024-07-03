package com.scala.core

import scala.util.{Try, Success, Failure}

sealed trait GraphLike[T, E <: EdgeLike[_ <: T]] {
    type Self[U] <: GraphLike[U, _ <: EdgeLike[_ <: U]]
    type Edge[U] <: EdgeLike[U]

    val vertices: Set[Vertex[_ <: T]]
    val edges: Set[E]

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

    def getNeighborsEdges[U <: T](v: Vertex[U]): Set[E] = {
        edges.filter((e) => (e.v1 == v || e.v2 == v))
    }

    def getNeighbors[U <: T](v: Vertex[U]): Set[Vertex[_ <: T]] = {
        getNeighborsEdges(v).map((e) => if (e.v1 == v) e.v2 else e.v1)
    }

    def getPredecessorsEdges[U <: T](v: Vertex[U]): Set[E]

    def getPredecessors[U <: T](v: Vertex[U]): Set[Vertex[_ <: T]] = {
        getPredecessorsEdges(v).map((e) => if (e.v1 == v) e.v2 else e.v1)
    }

    def getSuccessorsEdges[U <: T](v: Vertex[U]): Set[E]

    def getSuccessors[U <: T](v: Vertex[U]): Set[Vertex[_ <: T]] = {
        getSuccessorsEdges(v).map((e) => if (e.v1 == v) e.v2 else e.v1)
    }


    def hasCycle: Boolean = {
        @annotation.tailrec
        def _hasCycle(rest: Set[Vertex[_ <: T]], visited: Set[Vertex[_ <: T]]): Boolean = rest match {
            case (s: Set[Vertex[_ <: T]]) if s.nonEmpty => {
                val v = s.head
                val rs = s.tail
                if (visited.contains(v)) {
                    true
                }
                else {
                    _hasCycle(getSuccessors(v) ++ rs, visited + v)
                }
            }
            case _ => false
        }
        _hasCycle(vertices, Set())
    }


    def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[E]): Self[U]

    def copy[U <: T](newVertices: Set[Vertex[_ <: T]] = this.vertices, newEdges: Set[E] = this.edges): Self[U] = {
        create(newVertices, newEdges)
    }

    def serialize: String = ""
    def deserialize[U <: T](s: String): Self[U] = ???

    override def toString(): String = {
        s"Graph:\nVertices:\n${this.vertices}\nEdges:\n${edges}"
    }
}



sealed trait DirectedGraphLike[T, E <: DirectedEdgeLike[T]] extends GraphLike[T, E] {
    def getSources: Set[Vertex[_ <: T]] = {
        vertices.filter((v) => getPredecessors(v).isEmpty)
    }

    def getSinks: Set[Vertex[_ <: T]] = {
        vertices.filter((v) => getSuccessors(v).isEmpty)
    }

    def getSuccessorsEdges[U <: T](v: Vertex[U]): Set[E] = {
        edges.filter((e) => (e.v1 == v && e.direction == Direction.Forward)) ++
        edges.filter((e) => (e.v2 == v && e.direction == Direction.Backward))
    }

    def getPredecessorsEdges[U <: T](v: Vertex[U]): Set[E] = {
        edges.filter((e) => (e.v1 == v && e.direction == Direction.Backward)) ++
        edges.filter((e) => (e.v2 == v && e.direction == Direction.Forward))
    }

    def getRank[U <: T](v: Vertex[U]): Try[Int] = {
        if (hasCycle) Failure(new Exception("Graph has cycle"))
        else {
            val predecessors = getPredecessors(v)
            if (predecessors.isEmpty) {
                Success(0)
            }
            else {
                Success(predecessors.map(getRank(_).get).max + 1)
            }
        }


    }
}

sealed trait UndirectedGraphLike[T, E <: UndirectedEdgeLike[T]] extends GraphLike[T, E] {
    def getSuccessorsEdges[U <: T](v: Vertex[U]): Set[E] = {
        getNeighborsEdges(v)
    }

    def getPredecessorsEdges[U <: T](v: Vertex[U]): Set[E] = {
        getNeighborsEdges(v)
    }
}

sealed trait WeightedGraphLike[T, E <: WeightedEdgeLike[T]] extends GraphLike[T, E] {
}

sealed trait UnweightedGraphLike[T, E <: UnweightedEdgeLike[T]] extends GraphLike[T, E] {
}



case class DirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[DirectedEdge[T]]) extends DirectedGraphLike[T, DirectedEdge[T]] with UnweightedGraphLike[T, DirectedEdge[T]] {
    override type Self[U] = DirectedGraph[U]
    override type Edge[U] = DirectedEdge[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        DirectedGraph[U]()
    }
}

object DirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[DirectedEdge[T]]): DirectedGraph[T] = {
        new DirectedGraph[T](vertices, edges)
    }

    def apply[T](): DirectedGraph[T] = {
        new DirectedGraph[T](Set(), Set())
    }
}



case class UndirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[UndirectedEdge[T]]) extends UndirectedGraphLike[T, UndirectedEdge[T]] with UnweightedGraphLike[T, UndirectedEdge[T]] {
    override type Self[U] = UndirectedGraph[U]
    override type Edge[U] = UndirectedEdge[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        UndirectedGraph[U]()
    }
}

object UndirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[UndirectedEdge[T]]): UndirectedGraph[T] = {
        new UndirectedGraph[T](vertices, edges)
    }

    def apply[T](): UndirectedGraph[T] = {
        new UndirectedGraph[T](Set(), Set())
    }
}




case class WeightedDirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[WeightedDirectedEdge[T]]) extends DirectedGraphLike[T, WeightedDirectedEdge[T]] with WeightedGraphLike[T, WeightedDirectedEdge[T]] {
    override type Self[U] = WeightedDirectedGraph[U]
    override type Edge[U] = WeightedDirectedEdge[U]

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        WeightedDirectedGraph[U]()
    }

    def isSheduling: Boolean = {
        if (getSources.sizeIs > 1) false
        else if (getSources.exists(getSuccessorsEdges(_).exists(_.weight.get != 0))) false
        else if (getSinks.sizeIs > 1) false
        else if (vertices.exists(getSuccessorsEdges(_).map(_.weight.get).toSet.sizeIs > 1)) false
        else if (edges.exists(_.weight.get < 0)) false
        else true
    }

    def getEarliestDate[U <: T](v: Vertex[U]): Try[Int] = {
        if (!isSheduling) Failure(new Exception("Graph is not a scheduling graph"))
        else {
            val predecessors = getPredecessorsEdges(v)
            if (predecessors.isEmpty) {
                Success(0)
            }
            else {
                Success(predecessors.map((e) => getEarliestDate(if (e.v1 == v) e.v2 else e.v1).get + e.weight.get).max)
            }
        }
    }

    def getLatestDate[U <: T](v: Vertex[U]): Try[Int] = {
        if (!isSheduling) Failure(new Exception("Graph is not a scheduling graph"))
        else {
            val successors = getSuccessorsEdges(v)
            if (successors.isEmpty) {
                getEarliestDate(v)
            }
            else {
                Success(successors.map((e) => getLatestDate(if (e.v1 == v) e.v2 else e.v1).get - e.weight.get).min)
            }
        }
    }

    def getTotalFloat[U <: T](v: Vertex[U]): Try[Int] = {
        if (!isSheduling) Failure(new Exception("Graph is not a scheduling graph"))
        else {
            Success(getLatestDate(v).get - getEarliestDate(v).get)
        }
    }

    def getFreeFloat[U <: T](v: Vertex[U]): Try[Int] = {
        if (!isSheduling) Failure(new Exception("Graph is not a scheduling graph"))
        else {
            val successors = getSuccessorsEdges(v)
            if (successors.isEmpty) {
                Success(0)
            } else {
                Success(successors.map((e) => getEarliestDate(if (e.v1 == v) e.v2 else e.v1).get - getEarliestDate(v).get).min)
            }
        }

    }

    def getCriticalPaths[U <: T]: Try[Set[Set[Vertex[_ <: T]]]] = {      
        def _getCriticalPaths(paths: Set[Set[Vertex[_ <: T]]]): Set[Set[Vertex[_ <: T]]] = {
            val target = getSinks.head
            paths match {
                case p if p.isEmpty || p.exists(_.last == target) => p
                case p => {
                    val newPaths = p.flatMap((path) => {
                        val successors = edges.filter(
                            (e) => (e.v1 == path.last && e.direction == Direction.Forward && getTotalFloat(e.v2).get == 0 && getEarliestDate(e.v1).get + e.weight.get == getEarliestDate(e.v2).get) ||
                            (e.v2 == path.last && e.direction == Direction.Backward && getTotalFloat(e.v1).get == 0 && getEarliestDate(e.v2).get + e.weight.get == getEarliestDate(e.v1).get)
                        )
                        if (successors.nonEmpty) {
                            successors.map((e) => path + e.v2)
                        } else {
                            Set(path)
                        }
                    })
                    _getCriticalPaths(newPaths)
                }
            }
        }

        if (!isSheduling) Failure(new Exception("Graph is not a scheduling graph"))
        else {
            Success(_getCriticalPaths(getSources.map(Set(_))))
        }
    }
}

object WeightedDirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[WeightedDirectedEdge[T]]): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T](vertices, edges)
    }

    def apply[T](): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T](Set(), Set())
    }
}



case class WeightedUndirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[WeightedUndirectedEdge[T]]) extends UndirectedGraphLike[T, WeightedUndirectedEdge[T]] with WeightedGraphLike[T, WeightedUndirectedEdge[T]] {
    override type Self[U] = WeightedUndirectedGraph[U]
    override type Edge[U] = WeightedUndirectedEdge[U]
    
    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserialize[U <: T](s: String): Self[U] = {
        WeightedUndirectedGraph[U]()
    }
}

object WeightedUndirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[WeightedUndirectedEdge[T]]): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T](vertices, edges)
    }

    def apply[T](): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T](Set(), Set())
    }
}
