package com.scala.ui

import com.scala.core._

import zio._
import zio.http._
import zio.json._

object VertextRoutes {
  val routes = Routes(
        Method.PUT / Root -> handler((req: Request) => {
            for {
                body <- req.body.asString
                service <- ZIO.service[StateService]
                state <- service.getState[DirectedGraph[Int]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => body.fromJson[Vertex[Int]] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(vertex) => for {
                            _ <- service.setState(graph.addVertex(vertex))
                            s <- service.getState[DirectedGraph[Int]]
                            r <- s match {
                                case None => ZIO.succeed(Response.status(Status.InternalServerError))
                                case Some(g) => ZIO.succeed(Response.json(g.toJson))
                            }
                        } yield r
                    }
                }
            } yield response
        }).sandbox,
        Method.DELETE / Root -> handler((req: Request) => {
            for {
                body <- req.body.asString
                service <- ZIO.service[StateService]
                state <- service.getState[DirectedGraph[Int]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => body.fromJson[Vertex[Int]] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(vertex) => for {
                            _ <- service.setState(graph.removeVertex(vertex))
                            s <- service.getState[DirectedGraph[Int]]
                            r <- s match {
                                case None => ZIO.succeed(Response.status(Status.InternalServerError))
                                case Some(g) => ZIO.succeed(Response.json(g.toJson))
                            }
                        } yield r
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / Root -> handler((req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[DirectedGraph[Int]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(g) => ZIO.succeed(Response.json(g.getVertices.toJson))
                }
            } yield response
        }).sandbox,
    )
}