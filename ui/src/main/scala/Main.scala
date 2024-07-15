import com.scala.ui._

import com.scala.core._

import zio._
import zio.http._
import zio.http.codec.PathCodec.path

object GraphApp extends ZIOAppDefault {
  val routes = "directed" / {
    Root / GraphRoutes.routes[Int, DirectedEdge[Int], DirectedGraph[Int]] ++
    Root / DirectedGraphRoutes.routes[Int, DirectedEdge[Int], DirectedGraph[Int]]
  } ++
  "undirected" / {
    Root / GraphRoutes.routes[Int, UndirectedEdge[Int], UndirectedGraph[Int]]
  } ++
  "weighted-directed" / {
    Root / GraphRoutes.routes[Int, WeightedDirectedEdge[Int], WeightedDirectedGraph[Int]] ++
    Root / DirectedGraphRoutes.routes[Int, WeightedDirectedEdge[Int], WeightedDirectedGraph[Int]]
    Root / WeightedDirectedGraphRoutes.routes[Int]
  } ++
  "weighted-undirected" / {
    Root / GraphRoutes.routes[Int, WeightedUndirectedEdge[Int], WeightedUndirectedGraph[Int]]
  }

  def run = Server.serve(routes).provide(Server.default, StateService.layer)
}