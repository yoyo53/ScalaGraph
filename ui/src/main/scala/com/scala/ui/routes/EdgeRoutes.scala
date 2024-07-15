package com.scala.ui

import com.scala.core._

import zio._
import zio.http._
import zio.json._
import scala.reflect.ClassTag

object EdgeRoutes {
    def routes[T: JsonCodec, E <: EdgeLike[T] : JsonCodec, G <: GraphLike[T, E] : JsonCodec : ClassTag] = Routes(
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
        Method.POST / Root -> handler((req: Request) => {
            for {
                body <- req.body.asString
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => body.fromJson[Set[E]] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(edges) => graph.setEdges(edges) match {
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
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.clearEdges match {
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
            } yield response
        }).sandbox,
        Method.GET / "source" / int("source") -> handler((source: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == source) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getSuccessorsEdges(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.DELETE / "source" / int("source") -> handler((source: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == source) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.removeEdges(graph.getSuccessorsEdges(vertex)) match {
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
        Method.GET / "target" / int("target") -> handler((target: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == target) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getPredecessorsEdges(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.DELETE / "target" / int("target") -> handler((target: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == target) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.removeEdges(graph.getPredecessorsEdges(vertex)) match {
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
        Method.GET / "source" / int("source") / "target" / int("target") -> handler((source: Int, target: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == source) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(sourceVertex) => graph.getVertices.find(_.id == target) match {
                            case None => ZIO.succeed(Response.status(Status.NotFound))
                            case Some(targetVertex) => ZIO.succeed(Response.json(graph.getSuccessorsEdges(sourceVertex).filter(e => e.v1 == targetVertex || e.v2 == targetVertex).toJson))
                        }
                    }
                }
            } yield response
        }).sandbox,
        Method.DELETE / "source" / int("source") / "target" / int("target") -> handler((source: Int, target: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == source) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(sourceVertex) => graph.getVertices.find(_.id == target) match {
                            case None => ZIO.succeed(Response.status(Status.NotFound))
                            case Some(targetVertex) => graph.removeEdges(graph.getSuccessorsEdges(sourceVertex).filter(e => e.v1 == targetVertex || e.v2 == targetVertex)) match {
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
                }
            } yield response
        }).sandbox,
        Method.DELETE / "edge" -> handler((req: Request) => {
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
    )
}