package com.scala.core

import scala.util.{Try, Success, Failure}

extension [T](vertex: Vertex[T]) {
    def serializeGraphViz: String = {
        val data = vertex.data match {
            case Some(value) => s" [data=\"${value.toString}\"]"
            case None        => s""
        }

        s"${vertex.id}$data"
    }
}

extension (v: Vertex.type) {
    def deserializeGraphViz[T](vertexStr: String): Try[Vertex[T]] = {
        val vertexRegex = "([0-9]+)(?:\\s*?\\[.*?\\])?\\s*?;?\\n?".r
        vertexStr match {
            case vertexRegex(id) => Success(Vertex(id.toInt))
            case other => Failure(new Exception("Invalid vertex string: " + other))
        }
    }
}



extension [T](edge: EdgeLike[T]) {
    def serializeGraphViz: String = {
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

extension (v: EdgeLike.type) {
    def deserializeGraphViz[T](edgeStr: String): Try[EdgeLike[T]] = {
        val weightedRegex = "([0-9]+)\\s*?(->|--|<-)\\s*?([0-9]+)\\s*?\\[.*?label=([0-9]+).*?\\]\\s*?;?\\n?".r
        val unweightedRegex = "([0-9]+)\\s*?(->|--|<-)\\s*?([0-9]+)\\s*?;?\\n?".r
        edgeStr match {
            case weightedRegex(v1, "--", v2, weight) => Success(WeightedUndirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), weight.toInt))
            case weightedRegex(v1, "->", v2, weight) => Success(WeightedDirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Forward, weight.toInt))
            case weightedRegex(v1, "<-", v2, weight) => Success(WeightedDirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Backward, weight.toInt))
            case unweightedRegex(v1, "--", v2) => Success(UndirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt)))
            case unweightedRegex(v1, "->", v2) => Success(DirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Forward))
            case unweightedRegex(v1, "<-", v2) => Success(DirectedEdge[T](Vertex(v1.toInt), Vertex(v2.toInt), Direction.Backward))
            case other => Failure(new Exception("Invalid edge string: " + other))
        }
    }
}



extension [T](g: GraphLike[T, _ <: EdgeLike[_ <: T]]) {
    def serializeGraphViz: String = {
        val graph = g match {
            case _: DirectedGraphLike[?, ?] => "digraph"
            case _: UndirectedGraphLike[?, ?] => "graph"
        }

        val verticesStr = g.getVertices.foldLeft("")((acc, v) => acc + s"${v.serializeGraphViz};\n")
        val edgesStr = g.getEdges.foldLeft("")((acc, e) => acc + s"${e.serializeGraphViz};\n")
        
        s"$graph {\n$verticesStr$edgesStr}"
    }
}

extension (g: GraphLike.type) {
    def deserializeGraphViz[T, G <: GraphLike[T, _ <: EdgeLike[_ <: T]]](graphViz: String)(implicit construct: () => G): G = {
        val lines = graphViz.split("\n")
        
        val verticesLines = lines.tail.takeWhile(l => !l.contains("}") && !l.contains("->") && !l.contains("<-") && !l.contains("--"))
        val vertices = verticesLines.map(Vertex.deserializeGraphViz[T](_).get)

        val edgesLines = lines.tail.drop(verticesLines.length).takeWhile(!_.contains("}"))
        val edges = edgesLines.map(EdgeLike.deserializeGraphViz[T](_).get)

        val graph = vertices.foldLeft(construct())((acc, v) => acc.addVertex(v).asInstanceOf[G])
        edges.foldLeft(graph)((acc, e) => acc.addEdge(e).asInstanceOf[G])
    }
}



extension (g: DirectedGraph.type) {
    def deserializeGraphViz[T](graphViz: String): DirectedGraph[T] = {
        GraphLike.deserializeGraphViz(graphViz)(DirectedGraph[T])
    }
}

extension (g: UndirectedGraph.type) {
    def deserializeGraphViz[T](graphViz: String): UndirectedGraph[T] = {
        GraphLike.deserializeGraphViz(graphViz)(UndirectedGraph[T])
    }
}

extension (g: WeightedDirectedGraph.type) {
    def deserializeGraphViz[T](graphViz: String): WeightedDirectedGraph[T] = {
        GraphLike.deserializeGraphViz(graphViz)(WeightedDirectedGraph[T])
    }
}

extension (g: WeightedUndirectedGraph.type) {
    def deserializeGraphViz[T](graphViz: String): WeightedUndirectedGraph[T] = {
        GraphLike.deserializeGraphViz(graphViz)(WeightedUndirectedGraph[T])
    }
}