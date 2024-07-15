import com.scala.ui._

import com.scala.core._

import zio._
import zio.http._
import zio.http.codec.PathCodec._

object GraphApp extends ZIOAppDefault {

  val routes = "directed" / {
    Root / GraphRoutes.routes[Int, DirectedGraph[Int]] ++
    "vertex" / VertextRoutes.routes[Int, DirectedGraph[Int]] ++
    "edge" / EdgeRoutes.routes[Int, DirectedEdge[Int], DirectedGraph[Int]]
  } ++
  "undirected" / {
    Root / GraphRoutes.routes[Int, UndirectedGraph[Int]] ++
    "vertex" / VertextRoutes.routes[Int, UndirectedGraph[Int]] ++
    "edge" / EdgeRoutes.routes[Int, UndirectedEdge[Int], UndirectedGraph[Int]]
  } ++
  "weighted-directed" / {
    Root / GraphRoutes.routes[Int, WeightedDirectedGraph[Int]] ++
    "vertex" / VertextRoutes.routes[Int, WeightedDirectedGraph[Int]] ++
    "edge" / EdgeRoutes.routes[Int, WeightedDirectedEdge[Int], WeightedDirectedGraph[Int]]
  } ++
  "weighted-undirected" / {
    Root / GraphRoutes.routes[Int, WeightedUndirectedGraph[Int]] ++
    "vertex" / VertextRoutes.routes[Int, WeightedUndirectedGraph[Int]] ++
    "edge" / EdgeRoutes.routes[Int, WeightedUndirectedEdge[Int], WeightedUndirectedGraph[Int]]
  }

  def run = Server.serve(routes).provide(Server.default, StateService.layer)
}