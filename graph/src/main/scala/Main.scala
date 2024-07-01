import com.scala.graph._

object Main extends App {
  var graph = new DirectedGraph[Int]
  graph = graph.addVertex(Vertex(1))
  println(graph)
}