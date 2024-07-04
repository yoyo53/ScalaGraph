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
  
  println("Graph: " + graph)
  println()

  println("Graph Viz:\n" + graph.serializeGraphViz)
  println()

  val graphstr = graph.serializeJSON

  println("Graph JSON:\n" + graphstr)
  println()

  val graph2 = graph.deserializeJSON(graphstr)
  println("Graph 2:\n" + graph2)
  println()
}