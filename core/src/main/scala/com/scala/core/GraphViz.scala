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


implicit class GraphVizDeserializer(graphViz: String) {
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

    def fromGraphViz[G <: GraphLike[_, _ <: EdgeLike[_]]: ClassTag](implicit extractor: VertexTypeExtractor[G]): Try[G] = {
        val T = extractor.VertexType
        val regex = "(graph|digraph)\\s*?\\{((?:\\n|.)*?)\\}\\s*?".r
        val matches = regex.findFirstMatchIn(graphViz).map(m => (m.group(1), m.group(2)))
        val graphType = matches.map(_._1).get
        val content = matches.map(_._2)
        content match {
            case None => Failure(new Exception("Invalid graph string"))
            case Some(value) => {       
                val lines = value.split(";").map(_.strip).filter(_.nonEmpty)
                
                val verticesLines = lines.takeWhile(l => !l.contains("}") && !l.contains("->") && !l.contains("<-") && !l.contains("--"))
                val vertices = Try(verticesLines.map(_.fromGraphVizVertex[T.type]).map(_.get))
        
                val edgesLines = lines.drop(verticesLines.length).takeWhile(!_.contains("}"))
                val edges = Try(edgesLines.map(_.fromGraphVizEdge[T.type]).map(_.get))

                if (vertices.isFailure) Failure(vertices.failed.get)
                else if (edges.isFailure) Failure(edges.failed.get)
                else {
                    if (edges.get.isEmpty) {
                        graphType match {
                            case "digraph" => Success(DirectedGraph[T.type]().asInstanceOf[G])
                            case "graph" => Success(UndirectedGraph[T.type]().asInstanceOf[G])
                        }
                    }
                    else {
                        val weighted = edges.get.forall(_.isInstanceOf[WeightedEdgeLike[_]])                        
                        val directed = edges.get.forall(_.isInstanceOf[DirectedEdgeLike[_]])
                        
                        if (!weighted && edges.get.exists(_.isInstanceOf[WeightedEdgeLike[_]])) Failure(new Exception("Invalid edge type"))
                        else if (!directed && edges.get.exists(_.isInstanceOf[DirectedEdgeLike[_]])) Failure(new Exception("Invalid edge type"))
                        else {
                            val graph = (weighted, directed, graphType) match {
                                case (true, true, "digraph") => WeightedDirectedGraph[T.type]()
                                case (true, false, "graph") => WeightedUndirectedGraph[T.type]()
                                case (false, true, "digraph") => DirectedGraph[T.type]()
                                case (false, false, "graph") => UndirectedGraph[T.type]()
                                case _ => throw new Exception("Invalid graph type")
                            }

                            val verticesGraph = vertices.get.foldLeft(graph)((acc, v) => acc.addVertex(v).asInstanceOf[graph.type])
                            val fullGraph = edges.get.foldLeft(verticesGraph)((acc, e) => acc.addEdge(e.asInstanceOf[acc.E]).asInstanceOf[graph.type])
                
                            fullGraph match {
                                case g: G => Success(g.asInstanceOf[G])
                                case _ => Failure(new Exception("Invalid graph type"))
                            }
                        }
                    }
                }
            }
        }
    }
}

implicit def extractVertexType[G](implicit ct: ClassTag[G]): VertexTypeExtractor[G] = new VertexTypeExtractor[G]

class VertexTypeExtractor[G] {
    def VertexType[T](implicit ct: ClassTag[G]): Class[T] = {
        ct.runtimeClass.asInstanceOf[Class[T]]
    }
}