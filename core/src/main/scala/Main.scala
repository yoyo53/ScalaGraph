import com.scala.core._

object Main extends App {
  var graph = DirectedGraph[Int]()
  graph = graph.addVertex(Vertex(1))
  graph = graph.addVertex(Vertex(2))
  graph = graph.addVertex(Vertex(3))
  graph = graph.addVertex(Vertex(4))
  graph = graph.addEdge(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward))
  graph = graph.addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
  graph = graph.addEdge(DirectedEdge(Vertex(3), Vertex(4), Direction.Forward))
  graph = graph.addEdge(DirectedEdge(Vertex(3), Vertex(4), Direction.Backward))
  

  println(graph.vertices)
  println(graph.vertices.toList.map(graph.getRank))
  println(graph.hasCycle)
  
  println(graph)
}