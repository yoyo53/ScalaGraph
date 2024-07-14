package com.scala.ui

import com.scala.core._

import zio._
import zio.json._
import scala.reflect.ClassTag

sealed trait StateService {
    def setState[State <: GraphLike[_, _] : JsonEncoder : JsonDecoder](state: State): UIO[Unit]
    def getState[State <: GraphLike[_, _] : JsonEncoder : JsonDecoder : ClassTag]: UIO[Option[State]]
    def clearState: UIO[Unit]
}

object StateService {
    def layer: ULayer[StateService] = ZLayer.fromZIO {
        for {
            ref <- Ref.make[Option[GraphLike[_, _]]](None)
        } yield new StateService {
            override def setState[State <: GraphLike[_, _] : JsonEncoder : JsonDecoder](state: State): UIO[Unit] = ref.set(Some(state))
            override def getState[State <: GraphLike[_, _] : JsonEncoder : JsonDecoder : ClassTag]: UIO[Option[State]] = for {
                state <- ref.get
            }
            yield state match {
                case Some(s: State) => Some(s)
                case _ => None
            }
            override def clearState: UIO[Unit] = ref.set(None)
        }
    }
}

