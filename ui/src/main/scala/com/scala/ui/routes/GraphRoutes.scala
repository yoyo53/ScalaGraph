package com.scala.ui

import com.scala.core._

import zio._
import zio.http._
import zio.json._
import scala.reflect.ClassTag

object GraphRoutes {
    def routes[T : JsonCodec, G <: GraphLike[T, _ <: EdgeLike[T]] : JsonCodec : ClassTag](implicit constructor : () => G) = Routes(
        Method.GET / Root -> handler((req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(g) => ZIO.succeed(Response.json(g.toJson))
                }
            } yield response
        }).sandbox,
        Method.POST / Root -> handler((req: Request) => {
            for {
                service <- ZIO.service[StateService]
                _ <- service.setState(constructor())
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.InternalServerError))
                    case Some(g) => ZIO.succeed(Response.json(g.toJson))
                }
            } yield response
        }).sandbox,
        Method.DELETE / Root -> handler((req: Request) => {
            for {
                service <- ZIO.service[StateService]
                _ <- service.clearState
                response <- ZIO.succeed(Response.status(Status.NoContent))
            } yield response
        }).sandbox,
    )
}
