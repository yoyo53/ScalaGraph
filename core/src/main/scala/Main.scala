import com.scala.core._

import zio.json._

object Main extends App {  
  val graph = DirectedGraph[Int]().addVertex(Vertex(1))
    .addVertex(Vertex(2))
    .addVertex(Vertex(3))
    .addVertex(Vertex(4))
    .addVertex(Vertex(5))
    .addVertex(Vertex(6))
    .addVertex(Vertex(7))
    .addVertex(Vertex(8))
    .addVertex(Vertex(9, 12))
    .addEdge(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward))
    .addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
    .addEdge(DirectedEdge(Vertex(3), Vertex(4), Direction.Forward))
    .addEdge(DirectedEdge(Vertex(2), Vertex(5), Direction.Forward))
    .addEdge(DirectedEdge(Vertex(6), Vertex(7), Direction.Forward))
    .addEdge(DirectedEdge(Vertex(7), Vertex(8), Direction.Forward))
    .addEdge(DirectedEdge(Vertex(8), Vertex(3), Direction.Forward))
  
  println("Graph: " + graph)
  println()

  println("Graph Viz:\n" + graph.toGraphViz)
  println()

  val graphstr = graph.toJson

  println("Graph JSON:\n" + graphstr)
  println()

  val graph2 = graphstr.fromJson[DirectedGraph[Float]].getOrElse(DirectedGraph[Number]())
  println("Graph 2:\n" + graph2)
  println()

  val graph3 = graph.toGraphViz.fromGraphViz[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]())
  println("Graph 3:\n" + graph3.toJson)
  println()

  println("DFS: ")
  graph2.dfsTraverse(Vertex(1), v => println(v))

  println("BFS: ")
  graph2.bfsTraverse(Vertex(1), v => println(v))

  println("Has Cycle: " + graph2.hasCycle)

  println("Topological Sort: " + graph2.topologicalSort)

  println("Dijkstra: " + graph2.getShortestPathDijkstra(Vertex(6), Vertex(4)))
  println("Floyd Warshall: " + graph2.getShortestPathFloydWarshall(Vertex(6), Vertex(4)))
}