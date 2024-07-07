package com.scala.core

import scala.util.{Try, Success, Failure}
import scala.reflect.ClassTag

extension [T](vertex: Vertex[T]) {
    def toGraphViz: String = {
        val data = vertex.data match {
            case Some(value) => s" [data=\"${value.toString}\"]"
            case None        => s""
        }

        s"${vertex.id}$data"
    }
}


extension [T](edge: EdgeLike[T]) {
    def toGraphViz: String = {
        val weight = edge.weight match {
            case Some(value) => s" [label=$value]"
            case None => ""
        }
        val direction = edge.direction match {
            case Direction.Undirected => s"${edge.v1.id} -- ${edge.v2.id}"
            case Direction.Forward => s"${edge.v1.id} -> ${edge.v2.id}"
            case Direction.Backward => s"${edge.v2.id} -> ${edge.v1.id}"
        }

        s"$direction$weight"
    }
}


extension [T](g: GraphLike[T, _ <: EdgeLike[_ <: T]]) {
    def toGraphViz: String = {
        val graph = g match {
            case _: DirectedGraphLike[?, ?] => "digraph"
            case _: UndirectedGraphLike[?, ?] => "graph"
        }

        val verticesStr = g.getVertices.foldLeft("")((acc, v) => acc + s"${v.toGraphViz};")
        val edgesStr = g.getEdges.foldLeft("")((acc, e) => acc + s"${e.toGraphViz};")
        
        s"$graph {$verticesStr$edgesStr}"
    }

    def toGraphVizPretty: String = {
        val graph = g match {
            case _: DirectedGraphLike[?, ?] => "digraph"
            case _: UndirectedGraphLike[?, ?] => "graph"
        }

        val verticesStr = g.getVertices.foldLeft("")((acc, v) => acc + s"    ${v.toGraphViz};\n")
        val edgesStr = g.getEdges.foldLeft("")((acc, e) => acc + s"    ${e.toGraphViz};\n")
        
        s"$graph {\n$verticesStr$edgesStr}"
    }
}

extension (graphViz: String) {
    def fromGraphVizVertex[T]: Try[Vertex[T]] = { 
        val vertexRegex = "([0-9]+)(?:\\s*?\\[.*?\\])?\\s*?;?\\n?".r
        graphViz match {
            case vertexRegex(id) => Success(Vertex(id.toInt))
            case other => Failure(new Exception("Invalid vertex string: " + other))
        }
    }

    def fromGraphVizEdge[T]: Try[EdgeLike[T]] = {
        val weightedRegex = "([0-9]+)\\s*?(->|--|<-)\\s*?([0-9]+)\\s*?\\[.*?label=([0-9]+).*?\\]\\s*?;?\\n?".r
        val unweightedRegex = "([0-9]+)\\s*?(->|--|<-)\\s*?([0-9]+)\\s*?;?\\n?".r
        graphViz match {
            case weightedRegex(v1, "--", v2, weight) => Success(WeightedUndirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), weight.toInt))
            case weightedRegex(v1, "->", v2, weight) => Success(WeightedDirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Forward, weight.toInt))
            case weightedRegex(v1, "<-", v2, weight) => Success(WeightedDirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Backward, weight.toInt))
            case unweightedRegex(v1, "--", v2) => Success(UndirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt)))
            case unweightedRegex(v1, "->", v2) => Success(DirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Forward))
            case unweightedRegex(v1, "<-", v2) => Success(DirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Backward))
            case other => Failure(new Exception("Invalid edge string: " + other))
        }
    }

    def fromGraphViz[G <: GraphLike[_, _ <: EdgeLike[_]]: ClassTag]: Try[G] = {
        def _extractVertexType[T](implicit ev: G <:< GraphLike[_ <: T, _ <: EdgeLike[_ <: T]], ct: ClassTag[T]): Class[T] = {
            ct.runtimeClass.asInstanceOf[Class[T]]
        }

        val T = _extractVertexType
        val regex = "(graph|digraph)\\s*?\\{((?:\\n|.)*?)\\}\\s*?".r
        val matches = regex.findFirstMatchIn(graphViz).map(m => (m.group(1), m.group(2)))
        val graphType = matches.map(_._1).get
        val content = matches.map(_._2)
        content match {
            case None => Failure(new Exception("Invalid graph string"))
            case Some(value) => {       
                val lines = value.split(";").map(_.strip).filter(_.nonEmpty).toList
                
                val verticesLines = lines.takeWhile(l => !l.contains("}") && !l.contains("->") && !l.contains("<-") && !l.contains("--"))
                val vertices = Try(verticesLines.map(_.fromGraphVizVertex[T.type]).map(_.get))
        
                val edgesLines = lines.drop(verticesLines.length).takeWhile(!_.contains("}"))
                val edges = Try(edgesLines.map(_.fromGraphVizEdge[T.type]).map(_.get))

                if (vertices.isFailure) Failure(vertices.failed.get)
                else edges match {
                    case Failure(exception) => Failure(exception)
                    case Success(Nil) => graphType match {
                        case "digraph" => Success(DirectedGraph[T.type]())
                        case "graph" => Success(UndirectedGraph[T.type]())
                        case _ => Failure(new Exception("Invalid graph type"))
                    }
                    case Success(value) => {
                        val weighted = value.map(_.isInstanceOf[WeightedEdgeLike[_]])                        
                        val directed = value.map(_.isInstanceOf[DirectedEdgeLike[_]])

                        if (weighted.exists(_ != weighted.head)) Failure(new Exception("Invalid edge type"))
                        else if (directed.exists(_ != directed.head)) Failure(new Exception("Invalid edge type"))
                        else (weighted.head, directed.head, graphType) match {
                            case (true, true, "digraph") => Success(WeightedDirectedGraph[T.type]())
                            case (true, false, "graph") => Success(WeightedUndirectedGraph[T.type]())
                            case (false, true, "digraph") => Success(DirectedGraph[T.type]())
                            case (false, false, "graph") => Success(UndirectedGraph[T.type]())
                            case _ => Failure(new Exception("Invalid graph type"))
                        }
                    }
                } match {
                    case Success(graph: G) => {
                        val verticesGraph = vertices.get.foldLeft(graph)((acc, v) => acc.addVertex(v.asInstanceOf[acc.V]).asInstanceOf[G])
                        val fullGraph = edges.get.foldLeft(verticesGraph)((acc, e) => acc.addEdge(e.asInstanceOf[acc.E]).asInstanceOf[G])
            
                        Success(fullGraph)
                    }
                    case Failure(exception) => Failure(exception)
                    case g => Failure(new Exception("Invalid graph type"))
                }
            }
        }
    }
}