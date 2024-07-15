package com.scala.core

import org.scalatest._
import zio.json._

class VertexSpec extends UnitSpec {
  "A Vertex" should "be instantiated with an id and optional data" in {
    val vertexWithoutData = Vertex(1)
    vertexWithoutData.id should be(1)
    vertexWithoutData.data should be(None)

    val vertexWithData = Vertex(2, "testData")
    vertexWithData.id should be(2)
    vertexWithData.data should be(Some("testData"))
  }



  it should "be equal to another vertex with the same id and no data" in {
    val vertex1 = Vertex(1)
    val vertex2 = Vertex(1)

    vertex1 shouldEqual vertex2
  }

  it should "be equal to another vertex with the same id and data" in {
    val vertex1 = Vertex(1, "data")
    val vertex2 = Vertex(1, "data")

    vertex1 shouldEqual vertex2
  }

  it should "not be equal to another vertex with a different id and no data" in {
    val vertex1 = Vertex(1)
    val vertex2 = Vertex(2)

    vertex1 should not equal vertex2
  }

  it should "not be equal to another vertex with a different id or data" in {
    val vertex1 = Vertex(1, "data")
    val vertex2 = Vertex(2, "data")
    val vertex3 = Vertex(1, "differentData")

    vertex1 should not equal vertex2
    vertex1 should not equal vertex3
  }

  it should "not be equal to another vertex with a different data type" in {
    val vertex1 = Vertex(1, "data")
    val vertex2 = Vertex(1)
    val vertex3 = Vertex(1, 1)

    vertex1 should not equal vertex2
    vertex1 should not equal vertex3
  }



  it should "infer the data type when instantiated with an id and data" in {
    val vertex = Vertex(1, "data")
    val vertex2 = Vertex(1, 1)

    vertex.data.get shouldBe a [String]
    vertex2.data.get shouldBe a [Int]
  }

  it should "be equal to another vertex with the data type explicitly specified" in {
    val vertex = Vertex(1, "data")
    val vertex2 = Vertex[String](1, "data")

    vertex shouldEqual vertex2
  }



  it should "be equal to another vertex with the same id and no data when serialized to JSON" in {
    val vertex = Vertex[String](1)
    val vertex2 = Vertex[String](1)
    
    val vertexStr = vertex.toJson
    val vertex2Str = vertex2.toJson
    
    vertexStr shouldEqual vertex2Str
  }

  it should "be equal to another vertex with the same id and data when serialized to JSON" in {
    val vertex = Vertex(1, "data")
    val vertex2 = Vertex(1, "data")

    val vertexStr = vertex.toJson
    val vertex2Str = vertex2.toJson

    vertexStr shouldEqual vertex2Str
  }

  it should "not be equal to another vertex with a different id and no data when serialized to JSON" in {
    val vertex = Vertex[String](1)
    val vertex2 = Vertex[String](2)

    val vertexStr = vertex.toJson
    val vertex2Str = vertex2.toJson

    vertexStr should not equal vertex2Str
  }

  it should "not be equal to another vertex with a different id or data when serialized to JSON" in {
    val vertex = Vertex(1, "data")
    val vertex2 = Vertex(2, "data")
    val vertex3 = Vertex(1, "differentData")

    val vertexStr = vertex.toJson
    val vertex2Str = vertex2.toJson
    val vertex3Str = vertex3.toJson

    vertexStr should not equal vertex2Str
    vertexStr should not equal vertex3Str
  }

  it should "not be equal to another vertex with a different data type when serialized to JSON" in {
    val vertex = Vertex(1, "data")
    val vertex2 = Vertex[String](1)
    val vertex3 = Vertex(1, 1)

    val vertexStr = vertex.toJson
    val vertex2Str = vertex2.toJson
    val vertex3Str = vertex3.toJson

    vertexStr should not equal vertex2Str
    vertexStr should not equal vertex3Str
  }



  it should "be equal to another vertex with the same id and no data when deserialized from JSON" in {
    val vertexStr = "{\"id\":1}"
    val vertex = vertexStr.fromJson[Vertex[String]].getOrElse(null)
    val vertex2 = vertexStr.fromJson[Vertex[String]].getOrElse(null)

    vertex shouldEqual vertex2
  }

  it should "be equal to another vertex with the same id and data when deserialized from JSON" in {
    val vertexStr = "{\"id\":1,\"data\":\"data\"}"
    val vertex = vertexStr.fromJson[Vertex[String]].getOrElse(null)
    val vertex2 = vertexStr.fromJson[Vertex[String]].getOrElse(null)

    vertex shouldEqual vertex2
  }

  it should "not be equal to another vertex with a different id and no data when deserialized from JSON" in {
    val vertexStr = "{\"id\":1}"
    val vertexStr2 = "{\"id\":2}"
    val vertex = vertexStr.fromJson[Vertex[String]].getOrElse(null)
    val vertex2 = vertexStr2.fromJson[Vertex[String]].getOrElse(null)

    vertex should not equal vertex2
  }

  it should "not be equal to another vertex with a different id or data when deserialized from JSON" in {
    val vertexStr = "{\"id\":1,\"data\":\"data\"}"
    val vertexStr2 = "{\"id\":2,\"data\":\"data\"}"
    val vertexStr3 = "{\"id\":1,\"data\":\"differentData\"}"
    val vertex = vertexStr.fromJson[Vertex[String]].getOrElse(null)
    val vertex2 = vertexStr2.fromJson[Vertex[String]].getOrElse(null)
    val vertex3 = vertexStr3.fromJson[Vertex[String]].getOrElse(null)

    vertex should not equal vertex2
    vertex should not equal vertex3
  }

  it should "not be equal to another vertex with a different data type when deserialized from JSON" in {
    val vertexStr = "{\"id\":1,\"data\":\"data\"}"
    val vertexStr2 = "{\"id\":1}"
    val vertexStr3 = "{\"id\":1,\"data\":1}"
    val vertex = vertexStr.fromJson[Vertex[String]].getOrElse(null)
    val vertex2 = vertexStr2.fromJson[Vertex[String]].getOrElse(null)
    val vertex3 = vertexStr3.fromJson[Vertex[String]].getOrElse(null)

    vertex should not equal vertex2
    vertex should not equal vertex3
  }



  it should "be equal to another vertex with the same id and no data when serialized and deserialized from JSON" in {
    val vertex = Vertex[String](1)
    val vertex2 = vertex.toJson.fromJson[Vertex[String]].getOrElse(null)

    vertex shouldEqual vertex2
  }

  it should "be equal to another vertex with the same id and data when serialized and deserialized from JSON" in {
    val vertex = Vertex(1, "data")
    val vertex2 = vertex.toJson.fromJson[Vertex[String]].getOrElse(null)

    vertex shouldEqual vertex2
  }
}