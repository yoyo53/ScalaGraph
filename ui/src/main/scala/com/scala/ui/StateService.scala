package com.scala.ui

import com.scala.core._

import zio._
import zio.json._
import scala.reflect.ClassTag

sealed trait StateService {
    def setState[State <: GraphLike[_, _] : JsonCodec](state: State): UIO[Unit]
    def getState[State <: GraphLike[_, _] : JsonCodec : ClassTag]: UIO[Option[State]]
    def clearState: UIO[Unit]
}

object StateService {
    def layer: ULayer[StateService] = ZLayer.fromZIO {
        for {
            ref <- Ref.make[Option[GraphLike[_, _]]](None)
        } yield new StateService {
            def setState[State <: GraphLike[_, _] : JsonCodec](state: State): UIO[Unit] = ref.set(Some(state))
            def getState[State <: GraphLike[_, _] : JsonCodec : ClassTag]: UIO[Option[State]] = for {
                state <- ref.get
                result <- state match {
                    case Some(s: State) => ZIO.succeed(Some(s))
                    case _ => ZIO.succeed(None)
                }
            } yield result
            def clearState: UIO[Unit] = ref.set(None)
        }
    }
}

