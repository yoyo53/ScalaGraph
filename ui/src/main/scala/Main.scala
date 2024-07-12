import com.scala.core._

import zio._
import zio.http._
import zio.json._

trait StateService {
  def setState[State <: GraphLike[_, _ <: EdgeLike[_]] : JsonEncoder : JsonDecoder](state: State): UIO[Unit]
  def getState[State <: GraphLike[_, _ <: EdgeLike[_]] : JsonEncoder : JsonDecoder]: UIO[State]
}

object StateService {
  def layer: ULayer[StateService] = ZLayer.fromZIO {
    for {
      ref <- Ref.make[GraphLike[_, _ <: EdgeLike[_]]](null)
    } yield new StateService {
      override def setState[State <: GraphLike[_, _ <: EdgeLike[_]] : JsonEncoder : JsonDecoder](state: State): UIO[Unit] = ref.set(state)
      override def getState[State <: GraphLike[_, _ <: EdgeLike[_]] : JsonEncoder : JsonDecoder]: UIO[State] = ref.get.asInstanceOf[UIO[State]]
    }
  }
}

object GraphApp extends ZIOAppDefault {
  val routes =
    Routes(
      Method.GET / Root -> handler((req: Request) => {
        for {
          service <- ZIO.service[StateService]
          state <- service.getState[DirectedGraph[Int]]
        } yield state match {
          case null => Response.status(Status.NotFound)
          case g => Response.json(g.toJson)
        }
      }).sandbox,
      Method.POST / Root -> handler((req: Request) => {
        for {
          service <- ZIO.service[StateService]
          _ <- service.setState(DirectedGraph[Int]())
          state <- service.getState[DirectedGraph[Int]]
        } yield Response.json(state.toJson)
      }).sandbox,
      Method.PUT / Root / "vertex" -> handler((req: Request) => {
        for {
          body <- req.body.asString
          service <- ZIO.service[StateService]
          state <- service.getState[DirectedGraph[Int]]
          vertex = body.fromJson[Vertex[Int]].getOrElse(null)
          _ <- service.setState(state.addVertex(vertex))
          newState <- service.getState[DirectedGraph[Int]]
        } yield Response.json(newState.toJson)
      }).sandbox,
      Method.DELETE / Root / "vertex" -> handler((req: Request) => {
        for {
          body <- req.body.asString
          service <- ZIO.service[StateService]
          state <- service.getState[DirectedGraph[Int]]
          vertex = body.fromJson[Vertex[Int]].getOrElse(null)
          _ <- service.setState(state.removeVertex(vertex))
          newState <- service.getState[DirectedGraph[Int]]
        } yield Response.json(newState.toJson)
      }).sandbox,
    )

  def run = Server.serve(routes).provide(Server.default, StateService.layer)
}