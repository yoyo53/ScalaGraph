import com.scala.graph._

object Main extends App {
  var graph = new DirectedGraph[Int]
  graph = graph.addVertex(Vertex(1))
  graph = graph.addVertex(Vertex(2))
  graph = graph.addEdge(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward))
  println(graph)
}