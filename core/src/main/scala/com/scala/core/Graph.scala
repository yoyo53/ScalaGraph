package com.scala.core

import scala.util.{Try, Success, Failure}
import zio.json._

sealed trait GraphLike[T, E <: EdgeLike[_ <: T]] {
    type Self[U] <: GraphLike[U, _ <: EdgeLike[_ <: U]]
    type Edge[U] <: EdgeLike[U]

    protected val vertices: Set[Vertex[_ <: T]]
    protected val edges: Set[E]

    implicit val verticesEncoder: JsonEncoder[Set[Vertex[_ <: T]]] = JsonEncoder[Set[Vertex[T]]].contramap(_.asInstanceOf[Set[Vertex[T]]])
    implicit val edgesEncoder: JsonEncoder[Set[E]]
    implicit val selfEncoder : JsonEncoder[Self[T]]

    implicit val verticesDecoder: JsonDecoder[Set[Vertex[_ <: T]]] = JsonDecoder[Set[Vertex[T]]].map(_.asInstanceOf[Set[Vertex[_ <: T]]])
    implicit val edgesDecoder: JsonDecoder[Set[E]]
    implicit val selfDecoder: JsonDecoder[Self[T]]

    def getVertices: Set[Vertex[_ <: T]] = vertices

    def addVertex[U <: T](v: Vertex[U]): Self[U] = {
        copy(newVertices = this.vertices + v)
    }

    def removeVertex[U <: T](v: Vertex[U]): Self[U] = {
        copy(newVertices = this.vertices - v)
    }

    def getEdges: Set[E] = edges

    def addEdge[U <: T](e: E): Self[U] = {
        copy(newEdges = this.edges + e)
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

    def serializeJSON: String
    def deserializeJSON(s: String): Self[T]

    override def toString(): String = {
        s"Graph:\nVertices:\n${this.vertices}\nEdges:\n${edges}"
    }
}



sealed trait DirectedGraphLike[T, E <: DirectedEdgeLike[_ <: T]] extends GraphLike[T, E] {
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

sealed trait UndirectedGraphLike[T, E <: UndirectedEdgeLike[_ <: T]] extends GraphLike[T, E] {
    def getSuccessorsEdges[U <: T](v: Vertex[U]): Set[E] = {
        getNeighborsEdges(v)
    }

    def getPredecessorsEdges[U <: T](v: Vertex[U]): Set[E] = {
        getNeighborsEdges(v)
    }
}

sealed trait WeightedGraphLike[T, E <: WeightedEdgeLike[_ <: T]] extends GraphLike[T, E] {
}

sealed trait UnweightedGraphLike[T, E <: UnweightedEdgeLike[_ <: T]] extends GraphLike[T, E] {
}



case class DirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[DirectedEdge[_ <: T]]) extends DirectedGraphLike[T, DirectedEdge[_ <: T]] with UnweightedGraphLike[T, DirectedEdge[_ <: T]] {
    override type Self[U] = DirectedGraph[U]
    override type Edge[U] = DirectedEdge[U]

    override implicit val edgesEncoder: JsonEncoder[Set[Edge[_ <: T]]] = JsonEncoder[Set[EdgeLike[T]]].contramap(_.asInstanceOf[Set[EdgeLike[T]]])
    override implicit val selfEncoder: JsonEncoder[Self[T]] = EncoderWithType(this.getClass.getSimpleName)(DeriveJsonEncoder.gen[Self[T]])
    override implicit val edgesDecoder: JsonDecoder[Set[Edge[_ <: T]]] = JsonDecoder[Set[EdgeLike[T]]].map(_.asInstanceOf[Set[Edge[_ <: T]]])
    override implicit val selfDecoder: JsonDecoder[Self[T]] = DecoderWithType(this.getClass.getSimpleName)(DeriveJsonDecoder.gen[Self[T]])


    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[_ <: T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserializeJSON(s: String): Self[T] = {
        s.fromJson[Self[T]].getOrElse(throw new Exception("Failed to deserialize Graph"))
    }

    override def serializeJSON: String = {
        this.toJson
    }
}

object DirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[DirectedEdge[_ <: T]]): DirectedGraph[T] = {
        new DirectedGraph[T](vertices, edges)
    }

    def apply[T](): DirectedGraph[T] = {
        new DirectedGraph[T](Set(), Set())
    }
}

extension [T](g: DirectedGraph[T]) {
    def serializeGraphViz: String = {
        val start = "digraph G {\n"
        val verticesStr = g.vertices.foldLeft(start)((acc, v) => acc + s"${v.id} [data=${v.data.getOrElse("null")}];\n")
        val edgesStr = g.edges.foldLeft(verticesStr)((acc, e) => acc + s"${e.v1.id} -> ${e.v2.id};\n")
        s"$edgesStr}"
    }
}



case class UndirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[UndirectedEdge[_ <: T]]) extends UndirectedGraphLike[T, UndirectedEdge[_ <: T]] with UnweightedGraphLike[T, UndirectedEdge[_ <: T]] {
    override type Self[U] = UndirectedGraph[U]
    override type Edge[U] = UndirectedEdge[U]

    override implicit val edgesEncoder: JsonEncoder[Set[Edge[_ <: T]]] = JsonEncoder[Set[EdgeLike[T]]].contramap(_.asInstanceOf[Set[EdgeLike[T]]])
    override implicit val selfEncoder: JsonEncoder[Self[T]] = EncoderWithType(this.getClass.getSimpleName)(DeriveJsonEncoder.gen[Self[T]])
    override implicit val edgesDecoder: JsonDecoder[Set[Edge[_ <: T]]] = JsonDecoder[Set[EdgeLike[T]]].map(_.asInstanceOf[Set[Edge[_ <: T]]])
    override implicit val selfDecoder: JsonDecoder[Self[T]] = DecoderWithType(this.getClass.getSimpleName)(DeriveJsonDecoder.gen[Self[T]])

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[_ <: T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserializeJSON(s: String): Self[T] = {
        s.fromJson[Self[T]].getOrElse(throw new Exception("Failed to deserialize Graph"))
    }

    override def serializeJSON: String = {
        this.toJson
    }
}

object UndirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[UndirectedEdge[_ <: T]]): UndirectedGraph[T] = {
        new UndirectedGraph[T](vertices, edges)
    }

    def apply[T](): UndirectedGraph[T] = {
        new UndirectedGraph[T](Set(), Set())
    }
}

extension [T](g: UndirectedGraph[T]) {
    def serializeGraphViz: String = {
        val start = "graph G {\n"
        val verticesStr = g.vertices.foldLeft(start)((acc, v) => acc + s"${v.id} [data=${v.data.getOrElse("null")}];\n")
        val edgesStr = g.edges.foldLeft(verticesStr)((acc, e) => acc + s"${e.v1.id} -- ${e.v2.id};\n")
        s"$edgesStr}"
    }
}



case class WeightedDirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[WeightedDirectedEdge[_ <: T]]) extends DirectedGraphLike[T, WeightedDirectedEdge[_ <: T]] with WeightedGraphLike[T, WeightedDirectedEdge[_ <: T]] {
    override type Self[U] = WeightedDirectedGraph[U]
    override type Edge[U] = WeightedDirectedEdge[U]

    override implicit val edgesEncoder: JsonEncoder[Set[Edge[_ <: T]]] = JsonEncoder[Set[EdgeLike[T]]].contramap(_.asInstanceOf[Set[EdgeLike[T]]])
    override implicit val selfEncoder: JsonEncoder[Self[T]] = EncoderWithType(this.getClass.getSimpleName)(DeriveJsonEncoder.gen[Self[T]])
    override implicit val edgesDecoder: JsonDecoder[Set[Edge[_ <: T]]] = JsonDecoder[Set[EdgeLike[T]]].map(_.asInstanceOf[Set[Edge[_ <: T]]])
    override implicit val selfDecoder: JsonDecoder[Self[T]] = DecoderWithType(this.getClass.getSimpleName)(DeriveJsonDecoder.gen[Self[T]])

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[_ <: T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserializeJSON(s: String): Self[T] = {
        s.fromJson[Self[T]].getOrElse(throw new Exception("Failed to deserialize Graph"))
    }

    override def serializeJSON: String = {
        this.toJson
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
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[WeightedDirectedEdge[_ <: T]]): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T](vertices, edges)
    }

    def apply[T](): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T](Set(), Set())
    }
}

extension [T](g: WeightedDirectedGraph[T]) {
    def serializeGraphViz: String = {
        val start = "digraph G {\n"
        val verticesStr = g.vertices.foldLeft(start)((acc, v) => acc + s"${v.id} [data=${v.data.getOrElse("null")}];\n")
        val edgesStr = g.edges.foldLeft(verticesStr)((acc, e) => acc + s"${e.v1.id} -> ${e.v2.id} [weight=${e.weight.get}];\n")
        s"$edgesStr}"
    }
}



case class WeightedUndirectedGraph[T](override val vertices: Set[Vertex[_ <: T]], override val edges: Set[WeightedUndirectedEdge[_ <: T]]) extends UndirectedGraphLike[T, WeightedUndirectedEdge[_ <: T]] with WeightedGraphLike[T, WeightedUndirectedEdge[_ <: T]] {
    override type Self[U] = WeightedUndirectedGraph[U]
    override type Edge[U] = WeightedUndirectedEdge[U]
    
    override implicit val edgesEncoder: JsonEncoder[Set[Edge[_ <: T]]] = JsonEncoder[Set[EdgeLike[T]]].contramap(_.asInstanceOf[Set[EdgeLike[T]]])
    override implicit val selfEncoder: JsonEncoder[Self[T]] = EncoderWithType(this.getClass.getSimpleName)(DeriveJsonEncoder.gen[Self[T]])
    override implicit val edgesDecoder: JsonDecoder[Set[Edge[_ <: T]]] = JsonDecoder[Set[EdgeLike[T]]].map(_.asInstanceOf[Set[Edge[_ <: T]]])
    override implicit val selfDecoder: JsonDecoder[Self[T]] = DecoderWithType(this.getClass.getSimpleName)(DeriveJsonDecoder.gen[Self[T]])

    override def create[U <: T](newVertices: Set[Vertex[_ <: T]], newEdges: Set[Edge[_ <: T]]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges).asInstanceOf[Self[U]]
    }

    override def deserializeJSON(s: String): Self[T] = {
        s.fromJson[Self[T]].getOrElse(throw new Exception("Failed to deserialize Graph"))
    }

    override def serializeJSON: String = {
        this.toJson
    }
}

object WeightedUndirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[WeightedUndirectedEdge[_ <: T]]): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T](vertices, edges)
    }

    def apply[T](): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T](Set(), Set())
    }
}

extension [T](g: WeightedUndirectedGraph[T]) {
    def serializeGraphViz: String = {
        val start = "graph G {\n"
        val verticesStr = g.vertices.foldLeft(start)((acc, v) => acc + s"${v.id} [data=${v.data.getOrElse("null")}];\n")
        val edgesStr = g.edges.foldLeft(verticesStr)((acc, e) => acc + s"${e.v1.id} -- ${e.v2.id} [weight=${e.weight.get}];\n")
        s"$edgesStr}"
    }
}