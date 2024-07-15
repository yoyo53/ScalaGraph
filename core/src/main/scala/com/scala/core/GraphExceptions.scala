package com.scala.core

class GraphException(message: String) extends Exception(message)

class NegativeEdgeException(message: String) extends GraphException(message)

class CycleException(message: String) extends GraphException(message)

class NegativeCycleException(message: String) extends GraphException(message)

class NotSchedulingException(message: String) extends GraphException(message)


