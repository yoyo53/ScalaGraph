package com.scala.core

case class Vertex[+T](val id: Int, val data: Option[T] = None)