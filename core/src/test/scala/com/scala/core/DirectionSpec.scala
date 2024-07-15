package com.scala.core

import org.scalatest._
import zio.json._

class DirectionSpec extends UnitSpec {
  "A Direction" should "have an inverse" in {
    val undirected: Direction = Direction.Undirected
    val forward: Direction = Direction.Forward
    val backward: Direction = Direction.Backward

    undirected.inverse shouldEqual undirected
    forward.inverse shouldEqual backward
    backward.inverse shouldEqual forward
  }

  it should "be serialized to JSON" in {
    val direction = Direction.Forward
    val encoded = direction.toJson

    encoded shouldEqual "\"Forward\""
  }

  it should "be deserialized from JSON" in {
    val json = "\"Forward\""
    val decoded = json.fromJson[Direction].getOrElse(null)

    decoded shouldEqual Direction.Forward
  }

  it should "be encoded and decoded from JSON" in {
    val direction = Direction.Forward
    val encoded = direction.toJson
    val decoded = encoded.fromJson[Direction].getOrElse(null)

    decoded shouldEqual direction
  }

  it should "not be decoded from an invalid string" in {
    val invalidString = "Invalid"
    val decoded = invalidString.fromJson[Direction].getOrElse(null)

    decoded shouldEqual null
  }

  "A DirectedDirection" should "have an inverse" in {
    val forward: DirectedDirection = Direction.Forward
    val backward: DirectedDirection = Direction.Backward
    
    forward.inverse shouldEqual backward
    backward.inverse shouldEqual forward
  }

    it should "be serialized to JSON" in {
        val direction: DirectedDirection = Direction.Forward
        val encoded = direction.toJson
    
        encoded shouldEqual "\"Forward\""
    }

    it should "be deserialized from JSON" in {
        val json = "\"Forward\""
        val decoded = json.fromJson[DirectedDirection].getOrElse(null)
    
        decoded shouldEqual Direction.Forward
    }

  it should "be encoded and decoded from JSON" in {
    val direction: DirectedDirection = Direction.Forward
    val encoded = direction.toJson
    val decoded = encoded.fromJson[DirectedDirection].getOrElse(null)

    decoded shouldEqual direction
  }

    it should "not be decoded from an invalid string" in {
        val invalidString = "\"Undirected\""
        val decoded = invalidString.fromJson[DirectedDirection]
    
        decoded.isLeft shouldEqual true
    }
}