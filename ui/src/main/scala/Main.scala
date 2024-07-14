import com.scala.ui._

import zio._
import zio.http._
import zio.http.codec.PathCodec._

object GraphApp extends ZIOAppDefault {
  val routes = {
    Root / GraphRoutes.routes ++
    "vertex" / VertextRoutes.routes
  }

  def run = Server.serve(routes).provide(Server.default, StateService.layer)
}