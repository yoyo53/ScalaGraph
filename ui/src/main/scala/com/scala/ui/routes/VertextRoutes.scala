package com.scala.ui

import com.scala.core._

import zio._
import zio.http._
import zio.json._
import scala.reflect.ClassTag
import scala.util.{Success, Failure}

object VertextRoutes {
    def routes[T : JsonCodec, G <: GraphLike[T, _ <: EdgeLike[T]] : JsonCodec : ClassTag] = Routes(
        Method.GET / Root -> handler((req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(g) => ZIO.succeed(Response.json(g.getVertices.toJson))
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
                    case Some(graph) => body.fromJson[graph.V] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(vertex) => graph.addVertex(vertex) match {
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
                    case Some(graph) => body.fromJson[Set[graph.V]] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(vertices) => graph.setVertices(vertices) match {
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
                    case Some(graph) => graph.clearVertices match {
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
        Method.GET / int("id") -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(vertex.toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.DELETE / int("id") -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.removeVertex(vertex) match {
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
        Method.GET / int("id") / "neighbors" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getNeighbors(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "degree" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getDegree(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
    )
}

object DirectedVertexRoutes {
    def routes[T : JsonCodec, G <: DirectedGraphLike[T, _ <: DirectedEdgeLike[T]] : JsonCodec : ClassTag] = Routes(
        Method.GET / int("id") / "successors" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getSuccessors(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "predecessors" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getPredecessors(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "indegree" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getInDegree(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "outdegree" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => ZIO.succeed(Response.json(graph.getOutDegree(vertex).toJson))
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "rank" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[G]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.getRank(vertex) match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(value) => ZIO.succeed(Response.json(value.toJson))
                        }
                    }
                }
            } yield response
        }).sandbox,
    )
}

object WeightedDirectedVertexRoutes {
    def routes[T : JsonCodec] = Routes(
        Method.GET / int("id") / "earliest-date" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[WeightedDirectedGraph[T]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.getEarliestDate(vertex) match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(value) => ZIO.succeed(Response.json(value.toJson))
                        }
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "latest-date" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[WeightedDirectedGraph[T]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.getLatestDate(vertex) match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(value) => ZIO.succeed(Response.json(value.toJson))
                        }
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "total-float" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[WeightedDirectedGraph[T]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.getTotalFloat(vertex) match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(value) => ZIO.succeed(Response.json(value.toJson))
                        }
                    }
                }
            } yield response
        }).sandbox,
        Method.GET / int("id") / "free-float" -> handler((id: Int, req: Request) => {
            for {
                service <- ZIO.service[StateService]
                state <- service.getState[WeightedDirectedGraph[T]]
                response <- state match {
                    case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                    case Some(graph) => graph.getVertices.find(_.id == id) match {
                        case None => ZIO.succeed(Response.status(Status.NotFound))
                        case Some(vertex) => graph.getFreeFloat(vertex) match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(value) => ZIO.succeed(Response.json(value.toJson))
                        }
                    }
                }
            } yield response
        }).sandbox,
    )
}