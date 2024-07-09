package com.scala.core

import zio.json._
import scala.util.{Try, Success, Failure}

sealed trait GraphLike[T, Edge <: EdgeLike[_ <: T]] {
    type Self[U] <: GraphLike[U, _ <: EdgeLike[_ <: U]]
    type E = Edge
    type V = Vertex[_ <: T]

    protected val vertices: Set[V]
    protected val edges: Set[E]

    def create[U <: T, F <: E](newVertices: Set[V], newEdges: Set[F]): Self[U]


    def copy[U <: T](newVertices: Set[V] = vertices, newEdges: Set[E] = edges): Self[U] = {
        create(newVertices, newEdges)
    }

    def getVertices: Set[V] = vertices

    def addVertex[U <: T](v: V): Self[U] = {
        create(this.vertices + v, this.edges)
    }

    def removeVertex[U <: T](v: V): Self[U] = {
        create(this.vertices - v, this.edges.filter((e) => e.v1 != v && e.v2 != v))
    }

    def getEdges: Set[E] = edges

    def addEdge[U <: T, F <: E](e: F): Self[U] = {
        create(this.vertices + e.v1 + e.v2, this.edges + e)
    }

    def removeEdge[U <: T, F <: E](e: F): Self[U] = {
        create(this.vertices, this.edges - e)
    }

    override def toString(): String = {
        s"Graph:\nVertices:\n${this.vertices}\nEdges:\n${edges}"
    }

    def getNeighborsEdges(v: V): Set[E] = {
        edges.filter((e) => (e.v1 == v || e.v2 == v))
    }

    def getNeighbors(v: V): Set[V] = {
        getNeighborsEdges(v).map((e) => if (e.v1 == v) e.v2 else e.v1)
    }

    def getPredecessorsEdges(v: V): Set[E]

    def getPredecessors(v: V): Set[V] = {
        getPredecessorsEdges(v).map((e) => if (e.v1 == v) e.v2 else e.v1)
    }

    def getSuccessorsEdges(v: V): Set[E]

    def getSuccessors(v: V): Set[V] = {
        getSuccessorsEdges(v).map((e) => if (e.v1 == v) e.v2 else e.v1)
    }

    def getDegree(v: V): Int = {
        getNeighbors(v).size
    }

    def getAdgacencyMatrix: List[List[Int]] = {
        val verticesList = vertices.toList
        verticesList.map((v) => {
            val successors = getSuccessorsEdges(v)
            verticesList.map((u) => {
                successors.count((e) => e.v1 == u || e.v2 == u)
            })
        })
    }

    def getIncidenceMatrix: List[List[Int]] = {
        val verticesList = vertices.toList
        val edgesList = edges.toList
        verticesList.map((v) => {
            edgesList.map((e) => {
                if ((e.v2 == v && e.direction != Direction.Backward) || (e.v1 == v && e.direction != Direction.Forward)) 1
                else if (e.v1 == v || e.v2 == v) -1
                else 0
            })
        })
    }

    def getDegreeMatrix: List[List[Int]] = {
        val verticesList = vertices.toList
        verticesList.map((v) => {
            verticesList.map((u) => {
                if (v == u) getDegree(v)
                else 0
            })
        })
    }

    def getLaplacianMatrix: List[List[Int]] = {
        val adjacencyMatrix = getAdgacencyMatrix
        val degreeMatrix = getDegreeMatrix
        adjacencyMatrix.zip(degreeMatrix).map((a, d) => a.zip(d).map((ai, di) => di - ai))
    }

    @annotation.tailrec
    private def _dfs(rest: List[V], visited: List[V], f: Option[V => Unit]): List[V] = {
        if (rest.isEmpty) return visited
        else if (visited.contains(rest.head)) _dfs(rest.tail, visited, f)
        else {
            f match {
                case Some(f) => f(rest.head)
                case None =>
            }
            _dfs(getSuccessors(rest.head).toList ++ rest.tail, visited :+ rest.head , f)
        }
    }

    def dfsTraverse(source: V, f: V => Unit): Unit = {
        _dfs(List(source), List.empty, Some(f))
    }

    def dfsSort: List[V] = {
        _dfs(vertices.toList, List.empty, None)
    }

    @annotation.tailrec
    private def _bfs(rest: List[V], visited: List[V], f: Option[V => Unit]): List[V] = {
        if (rest.isEmpty) return visited
        else if (visited.contains(rest.head)) _bfs(rest.tail, visited, f)
        else {
            f match {
                case Some(f) => f(rest.head)
                case None =>
            }
            _bfs(rest.tail ++ getSuccessors(rest.head).toList, visited :+ rest.head, f)
        }
    }

    def bfsTraverse(source: V, f: V => Unit): Unit = {
        _bfs(List(source), List.empty, Some(f))
    }

    def bfsSort: List[V] = {
        _bfs(vertices.toList, List.empty, None)
    }

    def hasCycle: Boolean

    def getShortestPathsFloydWarshall: Try[Map[(V, V), List[E]]] = {
        @annotation.tailrec
        def _getPaths(paths: Map[(V, V), Option[E]], source: V, dest: V, current: E, result: List[E]): List[E] = {
            val target = result match {
                case Nil => if (current.v1 == dest) current.v2 else current.v1
                case _ => if (current.v1 == result.head.v1) current.v2 else current.v1
            }
            paths((source, target)) match {
            case Some(next) if next != current => _getPaths(paths, source, dest, next, current :: result)
            case _ => current :: result
            }
        }

        val verticesList = vertices.toList

        val distances = verticesList.flatMap(source => {
            val successors = getSuccessorsEdges(source)
            verticesList.map(target => {
                if (source == target) {
                    (source, target) -> 0
                } else {
                    (source, target) -> successors.filter((e) => e.v1 == target || e.v2 == target).map(_.weight.getOrElse(1)).minOption.getOrElse(Int.MaxValue)
                }
            })
        }).toMap

        val paths = verticesList.flatMap(source => {
            val successors = getSuccessorsEdges(source)
            verticesList.map(target => {
                if (source == target) {
                    (source, target) -> None
                } else {
                    (source, target) -> successors.filter((e) => e.v1 == target || e.v2 == target).minByOption(_.weight.getOrElse(1))
                }
            })
        }).toMap

        val (finalDistances, finalPaths) = verticesList.foldLeft((distances, paths)) {
            case ((dist, path), k) => {
                verticesList.foldLeft((dist, path)) { 
                    case ((d, p), i) => {
                        verticesList.foldLeft((d, p)) {
                            case ((distances, paths), j) => {
                                val newDistance = (distances((i, k)), distances((k, j))) match {
                                    case (Int.MaxValue, _) | (_, Int.MaxValue) => Int.MaxValue
                                    case (dik, dkj) => dik + dkj
                                }

                                if (newDistance < distances((i, j))) {
                                    (distances.updated((i, j), newDistance), paths.updated((i, j), paths((k, j))))
                                } else {
                                    (distances, paths)
                                }
                            }
                        }
                    }
                }
            }
        }

        if (vertices.exists((v) => finalDistances((v, v)) < 0)) return Failure(new Exception("Graph has negative cycles"))
        else {
            val fullPaths = finalPaths.map((key, value) => value match {
                case None => key -> List.empty
                case Some(e) => key -> _getPaths(finalPaths, key._1, key._2, e, List.empty)
                })
    
            Success(fullPaths.filter((key, value) => value.nonEmpty))
        }
    }

    def getShortestPathFloydWarshall(source: V, dest: V): Try[List[E]] = {
        getShortestPathsFloydWarshall.map(_(source, dest))
    }

    def getShortestPathsDijkstra(source: V): Try[Map[V, List[E]]] = {
        @annotation.tailrec
        def _getShortestPathsDijkstra(rest: List[V], visited: List[V], paths: Map[V, List[E]]): Map[V, List[E]] = {
            if (rest.isEmpty) return paths
            else if (visited.contains(rest.head)) _getShortestPathsDijkstra(rest.tail, visited, paths)
            else {
                val predecessors = getPredecessorsEdges(rest.head)
                val minPath = predecessors.filter((e) => visited.contains(if (e.v1 == rest.head) e.v2 else e.v1)).minByOption((e) => paths(if (e.v1 == rest.head) e.v2 else e.v1).map(_.weight.getOrElse(1)).sum)
                val newPaths = minPath match {
                    case Some(e) => paths + (rest.head -> (paths(if (e.v1 == rest.head) e.v2 else e.v1) :+ e))
                    case None => paths
                }
                _getShortestPathsDijkstra(getSuccessors(rest.head).toList ++ rest.tail, visited :+ rest.head, newPaths)
            }
        }

        if (edges.exists(_.weight.getOrElse(0) < 0)) Failure(new Exception("Graph has negative edges"))
        else {
            Success(_getShortestPathsDijkstra(List(source), List.empty, Map(source -> List.empty)))
        }
    }

    def getShortestPathDijkstra(source: V, dest: V): Try[List[E]] = {
        getShortestPathsDijkstra(source).map(_(dest))
    }
}

object GraphLike {
}



sealed trait DirectedGraphLike[T, Edge <: DirectedEdgeLike[_ <: T]] extends GraphLike[T, Edge] {
    def getPredecessorsEdges(v: V): Set[E] = {
        edges.filter((e) => (e.v1 == v && e.direction == Direction.Backward)) ++
        edges.filter((e) => (e.v2 == v && e.direction == Direction.Forward))
    }
    
    def getSuccessorsEdges(v: V): Set[E] = {
        edges.filter((e) => (e.v1 == v && e.direction == Direction.Forward)) ++
        edges.filter((e) => (e.v2 == v && e.direction == Direction.Backward))
    }

    def hasCycle: Boolean = {
        @annotation.tailrec
        def _hasCycle(stack: List[V], temp: Set[V], perm: Set[V]): Boolean = {
            if (stack.isEmpty) {
                false
            } else if (getSuccessors(stack.head).forall(v => perm.contains(v))) {
                _hasCycle(stack.tail, temp - stack.head, perm + stack.head)
            } else if (perm.contains(stack.head)) {
                _hasCycle(stack.tail, temp, perm)
            } else if (temp.contains(stack.head)) {
                true
            } else {
                _hasCycle(getSuccessors(stack.head).toList ++ stack, temp + stack.head, perm)
            }
        }

        _hasCycle(vertices.toList, Set.empty, Set.empty)
    }

    def getSources: Set[V] = {
        vertices.filter((v) => getPredecessors(v).isEmpty)
    }

    def getSinks: Set[V] = {
        vertices.filter((v) => getSuccessors(v).isEmpty)
    }

    def getInDegree(v: V): Int = {
        getPredecessors(v).size
    }

    def getOutDegree(v: V): Int = {
        getSuccessors(v).size
    }

    def getInDegreeMatrix: List[List[Int]] = {
        val verticesList = vertices.toList
        verticesList.map((v) => {
            verticesList.map((u) => {
                if (v == u) getInDegree(v)
                else 0
            })
        })
    }

    def getOutDegreeMatrix: List[List[Int]] = {
        val verticesList = vertices.toList
        verticesList.map((v) => {
            verticesList.map((u) => {
                if (v == u) getOutDegree(v)
                else 0
            })
        })
    }

    def getInLaplacianMatrix: List[List[Int]] = {
        val inDegreeMatrix = getInDegreeMatrix
        val adjacencyMatrix = getAdgacencyMatrix
        adjacencyMatrix.zip(inDegreeMatrix).map((a, d) => a.zip(d).map((ai, di) => di - ai))
    }

    def getOutLaplacianMatrix: List[List[Int]] = {
        val outDegreeMatrix = getOutDegreeMatrix
        val adjacencyMatrix = getAdgacencyMatrix
        adjacencyMatrix.zip(outDegreeMatrix).map((a, d) => a.zip(d).map((ai, di) => di - ai))
    }

    def getRank(v: V): Try[Int] = {
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

    def topologicalSort: Try[List[V]] = {
        @annotation.tailrec
        def _topologicalSort(rest: List[V], visited: List[V]): List[V] = {
            if (rest.isEmpty) return visited
            else if (visited.contains(rest.head)) _topologicalSort(rest.tail, visited)
            else if (getPredecessors(rest.head).exists(!visited.contains(_))) {
                _topologicalSort(rest.tail :+ rest.head, visited)
            }
            else {
                _topologicalSort(getSuccessors(rest.head).toList ++ rest.tail, visited :+ rest.head)
            }
        }

        if (hasCycle) Failure(new Exception("Graph has cycle"))
        else {
            Success(_topologicalSort(getSources.toList, List.empty))
        }
    }
}

sealed trait UndirectedGraphLike[T, Edge <: UndirectedEdgeLike[_ <: T]] extends GraphLike[T, Edge] {
    def getPredecessorsEdges(v: V): Set[E] = {
        getNeighborsEdges(v)
    }

    def getSuccessorsEdges(v: V): Set[E] = {
        getNeighborsEdges(v)
    }

    def hasCycle: Boolean = {
        @annotation.tailrec
        def _hasCycle(stack: List[(V, Option[V])], temp: Set[V], perm: Set[V]): Boolean = {
            val (head, parent) = stack.head
            if (stack.isEmpty) {
                false
            } else if ((getSuccessors(head) - parent.getOrElse(null)).forall(perm.contains(_))) {
                _hasCycle(stack.tail, temp - head, perm + head)
            } else if (perm.contains(head)) {
                _hasCycle(stack.tail, temp, perm)
            } else if (temp.contains(head)) {
                true
            } else {
                val successors = getSuccessors(head) - parent.getOrElse(null)
                _hasCycle(successors.map(s => (s, Some(head))).toList ++ stack, temp + head, perm)
            }
        }


        _hasCycle(vertices.map((_, None)).toList, Set.empty, Set.empty)
    }
}

sealed trait WeightedGraphLike[T, Edge <: WeightedEdgeLike[_ <: T]] extends GraphLike[T, Edge] {
}

sealed trait UnweightedGraphLike[T, Edge <: UnweightedEdgeLike[_ <: T]] extends GraphLike[T, Edge] {
}



case class DirectedGraph[T](protected val vertices: Set[Vertex[_ <: T]], protected val edges: Set[DirectedEdge[_ <: T]]) extends DirectedGraphLike[T, DirectedEdge[_ <: T]] with UnweightedGraphLike[T, DirectedEdge[_ <: T]] {
    type Self[U] = DirectedGraph[U]

    def create[U <: T, F <: E](newVertices: Set[V], newEdges: Set[F]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges.asInstanceOf[Set[E]]).asInstanceOf[Self[U]]
    }
}

object DirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[DirectedEdge[_ <: T]]): DirectedGraph[T] = {
        new DirectedGraph[T](vertices, edges)
    }

    def apply[T](): DirectedGraph[T] = {
        new DirectedGraph[T](Set.empty, Set.empty)
    }

    implicit def encoder[T: JsonEncoder]: JsonEncoder[DirectedGraph[T]] = EncoderWithType(classOf[DirectedGraph[T]].getSimpleName)(DeriveJsonEncoder.gen[DirectedGraph[T]])
    implicit def decoder[T: JsonDecoder]: JsonDecoder[DirectedGraph[T]] = DecoderWithType(classOf[DirectedGraph[T]].getSimpleName)(DeriveJsonDecoder.gen[DirectedGraph[T]])
}



case class UndirectedGraph[T](protected val vertices: Set[Vertex[_ <: T]], protected val edges: Set[UndirectedEdge[_ <: T]]) extends UndirectedGraphLike[T, UndirectedEdge[_ <: T]] with UnweightedGraphLike[T, UndirectedEdge[_ <: T]] {
    type Self[U] = UndirectedGraph[U]

    def create[U <: T, F <: E](newVertices: Set[V], newEdges: Set[F]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges.asInstanceOf[Set[E]]).asInstanceOf[Self[U]]
    }
}

object UndirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[UndirectedEdge[_ <: T]]): UndirectedGraph[T] = {
        new UndirectedGraph[T](vertices, edges)
    }

    def apply[T](): UndirectedGraph[T] = {
        new UndirectedGraph[T](Set.empty, Set.empty)
    }

    implicit def encoder[T: JsonEncoder]: JsonEncoder[UndirectedGraph[T]] = EncoderWithType(classOf[UndirectedGraph[T]].getSimpleName)(DeriveJsonEncoder.gen[UndirectedGraph[T]])
    implicit def decoder[T: JsonDecoder]: JsonDecoder[UndirectedGraph[T]] = DecoderWithType(classOf[UndirectedGraph[T]].getSimpleName)(DeriveJsonDecoder.gen[UndirectedGraph[T]])
}



case class WeightedDirectedGraph[T](protected val vertices: Set[Vertex[_ <: T]], protected val edges: Set[WeightedDirectedEdge[_ <: T]]) extends DirectedGraphLike[T, WeightedDirectedEdge[_ <: T]] with WeightedGraphLike[T, WeightedDirectedEdge[_ <: T]] {
    type Self[U] = WeightedDirectedGraph[U]

    def create[U <: T, F <: E](newVertices: Set[V], newEdges: Set[F]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges.asInstanceOf[Set[E]]).asInstanceOf[Self[U]]
    }

    def isSheduling: Boolean = {
        if (getSources.sizeIs > 1) false
        else if (getSources.exists(getSuccessorsEdges(_).exists(_.weight.get != 0))) false
        else if (getSinks.sizeIs > 1) false
        else if (vertices.exists(getSuccessorsEdges(_).map(_.weight.get).toSet.sizeIs > 1)) false
        else if (edges.exists(_.weight.get < 0)) false
        else true
    }

    def getEarliestDate(v: V): Try[Int] = {
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

    def getLatestDate(v: V): Try[Int] = {
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

    def getTotalFloat(v: V): Try[Int] = {
        if (!isSheduling) Failure(new Exception("Graph is not a scheduling graph"))
        else {
            Success(getLatestDate(v).get - getEarliestDate(v).get)
        }
    }

    def getFreeFloat(v: V): Try[Int] = {
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

    def getCriticalPaths: Try[Set[List[E]]] = {      
        @annotation.tailrec
        def _getCriticalPaths(paths: Set[List[E]]): Set[List[E]] = {
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
                            successors.map((e) => path :+ e)
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
            Success(_getCriticalPaths(getSources.flatMap(getSuccessorsEdges(_)).map(List(_))))
        }
    }
}

object WeightedDirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[WeightedDirectedEdge[_ <: T]]): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T](vertices, edges)
    }

    def apply[T](): WeightedDirectedGraph[T] = {
        new WeightedDirectedGraph[T](Set.empty, Set.empty)
    }

    implicit def encoder[T: JsonEncoder]: JsonEncoder[WeightedDirectedGraph[T]] = EncoderWithType(classOf[WeightedDirectedGraph[T]].getSimpleName)(DeriveJsonEncoder.gen[WeightedDirectedGraph[T]])
    implicit def decoder[T: JsonDecoder]: JsonDecoder[WeightedDirectedGraph[T]] = DecoderWithType(classOf[WeightedDirectedGraph[T]].getSimpleName)(DeriveJsonDecoder.gen[WeightedDirectedGraph[T]])
}



case class WeightedUndirectedGraph[T](protected val vertices: Set[Vertex[_ <: T]], protected val edges: Set[WeightedUndirectedEdge[_ <: T]]) extends UndirectedGraphLike[T, WeightedUndirectedEdge[_ <: T]] with WeightedGraphLike[T, WeightedUndirectedEdge[_ <: T]] {
    type Self[U] = WeightedUndirectedGraph[U]
    
    def create[U <: T, F <: E](newVertices: Set[V], newEdges: Set[F]): Self[U] = {
        new Self[T](vertices = newVertices, edges = newEdges.asInstanceOf[Set[E]]).asInstanceOf[Self[U]]
    }
}

object WeightedUndirectedGraph {
    def apply[T](vertices: Set[Vertex[_ <: T]], edges: Set[WeightedUndirectedEdge[_ <: T]]): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T](vertices, edges)
    }

    def apply[T](): WeightedUndirectedGraph[T] = {
        new WeightedUndirectedGraph[T](Set.empty, Set.empty)
    }

    implicit def encoder[T: JsonEncoder]: JsonEncoder[WeightedUndirectedGraph[T]] = EncoderWithType(classOf[WeightedUndirectedGraph[T]].getSimpleName)(DeriveJsonEncoder.gen[WeightedUndirectedGraph[T]])
    implicit def decoder[T: JsonDecoder]: JsonDecoder[WeightedUndirectedGraph[T]] = DecoderWithType(classOf[WeightedUndirectedGraph[T]].getSimpleName)(DeriveJsonDecoder.gen[WeightedUndirectedGraph[T]])
}