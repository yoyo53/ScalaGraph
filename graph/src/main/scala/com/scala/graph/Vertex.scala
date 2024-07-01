package com.scala.graph

case class Vertex[+T](val id: Int, val data: Option[T] = None)