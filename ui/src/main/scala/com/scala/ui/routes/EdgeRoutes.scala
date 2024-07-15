package com.scala.ui

import com.scala.core._

import zio._
import zio.http._
import zio.json._
import scala.reflect.ClassTag

object EdgeRoutes {
      def routes[T: JsonCodec, E <: EdgeLike[T] : JsonCodec, G <: GraphLike[T, E] : JsonCodec : ClassTag] = Routes(
        Method.PUT / Root -> handler((req: Request) => {
            for {
                body <- req.body.asString
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => body.fromJson[E] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(edge) => graph.addEdge(edge) match {
                            case gr: G => for {
                                _ <- service.setState(gr)
                                s <- service.getState[G]
                                r <- s match {
                                    case None => ZIO.succeed(Response.status(Status.InternalServerError))
                                    case Some(g) => ZIO.succeed(Response.json(g.toJson))
                                }
                            } yield r
                            case _ => ZIO.succeed(Response.status(Status.InternalServerError))
                        } 
                    }
                }
            } yield response
        }).sandbox,
        Method.DELETE / Root -> handler((req: Request) => {
            for {
                body <- req.body.asString
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => body.fromJson[E] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(edge) => graph.removeEdge(edge) match {
                            case gr: G => for {
                                _ <- service.setState(gr)
                                s <- service.getState[G]
                                r <- s match {
                                    case None => ZIO.succeed(Response.status(Status.InternalServerError))
                                    case Some(g) => ZIO.succeed(Response.json(g.toJson))
                                }
                            } yield r
                            case _ => ZIO.succeed(Response.status(Status.InternalServerError))
                        }
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / Root -> handler((req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(g) => ZIO.succeed(Response.json(g.getEdges.toJson))
                }
            } yield response
        }).sandbox,
    )
}