package com.scala.ui

import com.scala.core._

import zio._
import zio.http._
import zio.http.codec.PathCodec.path
import zio.json._
import scala.reflect.ClassTag
import scala.util.{Failure, Success}

object GraphRoutes {
    def routes[T : JsonCodec, E <: EdgeLike[T] : JsonCodec, G <: GraphLike[T, E] : JsonCodec : ClassTag](implicit constructor : () => G) = {
        Root / Routes(
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
            Method.PUT / Root -> handler((req: Request) => {
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
            Method.POST / Root -> handler((req: Request) => {
                for {
                    body <- req.body.asString
                    service <- ZIO.service[StateService]
                    response <- body.fromJson[G] match {
                        case Left(value) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Right(gr) => for {
                            _ <- service.setState(gr)
                            s <- service.getState[G]
                            r <- s match {
                                case None => ZIO.succeed(Response.status(Status.InternalServerError))
                                case Some(g) => ZIO.succeed(Response.json(g.toJson))
                            }
                        } yield r
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
        ) ++
        "graph-viz" / Routes(
            Method.GET / Root -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.toGraphViz))
                    }
                } yield response
            }).sandbox,
            Method.POST / Root -> handler((req: Request) => {
                for {
                    body <- req.body.asString
                    service <- ZIO.service[StateService]
                    response <- body.fromGraphViz[G] match {
                        case Failure(_) => ZIO.succeed(Response.status(Status.BadRequest))
                        case Success(gr) => for {
                            _ <- service.setState(gr)
                            s <- service.getState[G]
                            r <- s match {
                                case None => ZIO.succeed(Response.status(Status.InternalServerError))
                                case Some(g) => ZIO.succeed(Response.json(g.toGraphViz))
                            }
                        } yield r
                    }
                } yield response
            }).sandbox,
        ) ++
        "matrix" / Routes(
            Method.GET / "adjacency" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getAdgacencyMatrix.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "incidence" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getIncidenceMatrix.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "degree" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getDegreeMatrix.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "laplacian" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getLaplacianMatrix.toJson))
                    }
                } yield response
            }).sandbox,
        ) ++
        "sort" / Routes(
            Method.GET / "dfs" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.dfsSort.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "bfs" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.bfsSort.toJson))
                    }
                } yield response
            }).sandbox,
        ) ++
        "cycle" / Routes(
            Method.GET / Root -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[G]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.hasCycle.toJson))
                    }
                } yield response
            }).sandbox,
        ) ++
        "path" / {
            "dijkstra" / Routes(
                Method.GET / "source" / int("source") -> handler((source: Int, req: Request) => {
                    for {
                        service <- ZIO.service[StateService]
                        state <- service.getState[G]
                        response <- state match {
                            case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                            case Some(graph) => graph.getVertices.find(_.id == source) match {
                                case None => ZIO.succeed(Response.status(Status.NotFound))
                                case Some(sourceVertex) => graph.getShortestPathsDijkstra(sourceVertex) match {
                                    case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                                    case Success(paths) => ZIO.succeed(Response.json(paths.toList.toJson))
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
                                    case Some(targetVertex) => graph.getShortestPathDijkstra(sourceVertex, targetVertex) match {
                                        case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                                        case Success(paths) => ZIO.succeed(Response.json(paths.toJson))
                                    }
                                }
                            }
                        }
                    } yield response
                }).sandbox,
            ) ++
            "floyd" / Routes(
                Method.GET / Root -> handler((req: Request) => {
                    for {
                        service <- ZIO.service[StateService]
                        state <- service.getState[G]
                        response <- state match {
                            case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                            case Some(graph) => graph.getShortestPathsFloydWarshall match {
                                case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                                case Success(paths) => ZIO.succeed(Response.json(paths.toList.toJson))
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
                                    case Some(targetVertex) => graph.getShortestPathFloydWarshall(sourceVertex, targetVertex) match {
                                        case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                                        case Success(paths) => ZIO.succeed(Response.json(paths.toJson))
                                    }
                                }
                            }
                        }
                    } yield response
                }).sandbox,
            )
        } ++
        "vertex" / VertextRoutes.routes[T, G] ++
        "edge" / EdgeRoutes.routes[T, E, G]
    }
}

object DirectedGraphRoutes {
    def routes[T : JsonCodec, E <: DirectedEdgeLike[T] : JsonCodec, G <: DirectedGraphLike[T, E] : JsonCodec : ClassTag] = {
        Routes(
            Method.GET / "sources" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getSources.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "sinks" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getSinks.toJson))
                    }
                } yield response
            }).sandbox,
        ) ++
        "matrix" / Routes(
            Method.GET / "indegree" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getInDegreeMatrix.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "outdegree" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getOutDegreeMatrix.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "inlaplacian" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getInLaplacianMatrix.toJson))
                    }
                } yield response
            }).sandbox,
            Method.GET / "outlaplacian" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.getOutLaplacianMatrix.toJson))
                    }
                } yield response
            }).sandbox,
        ) ++
        "sort" / Routes(
            Method.GET / "topological" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[DirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => g.topologicalSort match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(sort) => ZIO.succeed(Response.json(sort.toJson))
                        }
                    }
                } yield response
            }).sandbox,
        ) ++
        "vertex" / DirectedVertexRoutes.routes[T, G]
    }
}

object WeightedDirectedGraphRoutes {
    def routes[T : JsonCodec] = {
        Root / Routes(
            Method.GET / "sheduling" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[WeightedDirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => ZIO.succeed(Response.json(g.isSheduling.toJson))
                    }
                } yield response
            }).sandbox,
        ) ++
        "path" / Routes(
            Method.GET / "critical" -> handler((req: Request) => {
                for {
                    service <- ZIO.service[StateService]
                    state <- service.getState[WeightedDirectedGraph[T]]
                    response <- state match {
                        case None => ZIO.succeed(Response.status(Status.MethodNotAllowed))
                        case Some(g) => g.getCriticalPaths match {
                            case Failure(_) => ZIO.succeed(Response.status(Status.InternalServerError))
                            case Success(paths) => ZIO.succeed(Response.json(paths.toJson))
                        }
                    }
                } yield response
            }).sandbox,
        ) ++
        "vertex" / WeightedDirectedVertexRoutes.routes[T]
    }
}