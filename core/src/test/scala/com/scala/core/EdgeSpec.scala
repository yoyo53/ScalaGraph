package com.scala.core

import org.scalatest._
import zio.json._

class EdgeSpec extends UnitSpec {
    // An Edge have 4 forms:
    // 1. DirectedEdge
    // 2. UndirectedEdge
    // 3. WeightedDirectedEdge
    // 4. WeightedUndirectedEdge


    // 1. DirectedEdge

    // Argument changes
    "An DirectedEdge" should "be equal to another edge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Forward)

        edge1 shouldEqual edge2
    }

    it should "be not equal to another edge with the same vertices but different direction" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Backward)

        edge1 should not equal edge2
    }

    it should "be equal to another edge with different vertices and oposite direction" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex2, vertex1, Direction.Backward)

        edge1 shouldEqual edge2
    }

    it should "be not equal to another edge with different vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val vertex3 = Vertex(3)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex3, Direction.Forward)

        edge1 should not equal edge2
    }



    // With different Edge types

    it should "be not equal to an UndirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = UndirectedEdge(vertex1, vertex2)

        edge1 should not equal edge2
    }

    it should "be not equal to an WeightedDirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)

        edge1 should not equal edge2
    }

    it should "be not equal to an WeightedUndirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 1)

        edge1 should not equal edge2
    }

    
    // Test the JsonEncoder and JsonDecoder
    "An DirectedEdge JSON" should "be equal to another edge JSON with the same vertices and direction" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Forward)

        edge1.toJson shouldEqual edge2.toJson
    }

    it should "be equal not equal to another edge JSON with the same vertices but different direction" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Backward)

        edge1.toJson should not equal edge2.toJson
    }

    it should "be equal to another edge with the same vertices and direction" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = edge1.toJson.fromJson[DirectedEdge[String]].getOrElse(null)

        edge1 shouldEqual edge2
    }

    it should "be not equal to another edge with the same vertices but different direction" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Backward)

        edge1 should not equal edge2
    }



    // Test the HashCode
    "An DirectedEdge HashCode" should "be consistent with equals" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Forward)

        (edge1 == edge2) shouldEqual (edge1.hashCode == edge2.hashCode)
      }

      it should "differ for edges with different vertices or direction" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val vertex3 = Vertex(3, "data3")
        val edge1 = DirectedEdge(vertex1, vertex2, Direction.Forward)
        val edge2 = DirectedEdge(vertex1, vertex3, Direction.Forward)
        val edge3 = DirectedEdge(vertex1, vertex2, Direction.Backward)

        edge1.hashCode should not equal edge2.hashCode
        edge1.hashCode should not equal edge3.hashCode
      }







    // 2. UndirectedEdge
    // Argument changes
    "An UndirectedEdge" should "be equal to another edge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex1, vertex2)

        edge1 shouldEqual edge2
    }

    "An UndirectedEdge" should "be equal to another edge with the same vertices but in reverse order" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex2, vertex1)

        edge1 shouldEqual edge2
    }

    "An UndirectedEdge" should "be not equal to another edge with different vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val vertex3 = Vertex(3)
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex1, vertex3)

        edge1 should not equal edge2
    }



    // With different Edge types

    "An UndirectedEdge" should "be not equal to an DirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Forward)

        edge1 should not equal edge2
    }

    "An UndirectedEdge" should "be not equal to an WeightedDirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)

        edge1 should not equal edge2
    }


    "An UndirectedEdge" should "be not equal to an WeightedUndirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 1)

        edge1 should not equal edge2
    }

    // Test the JsonEncoder and JsonDecoder
    "An UndirectedEdge JSON" should "be equal to another edge JSON with the same vertices" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex1, vertex2)

        edge1.toJson shouldEqual edge2.toJson
    }

    it should "be equal not equal to another edge JSON with the same vertices but in reverse order" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex2, vertex1)

        edge1.toJson should not equal edge2.toJson
    }

    it should "be equal to another edge with the same vertices" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = edge1.toJson.fromJson[UndirectedEdge[String]].getOrElse(null)

        edge1 shouldEqual edge2
    }

    it should "be not equal to another edge with different vertices" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val vertex3 = Vertex(3,"data3")
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex1, vertex3)

        edge1 should not equal edge2
    }


    // Test the HashCode

    "An UndirectedEdge HashCode" should "be consistent with equals" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex1, vertex2)

        (edge1 == edge2) shouldEqual (edge1.hashCode == edge2.hashCode)
      }

      it should "differ for edges with different vertices" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val vertex3 = Vertex(3, "data3")
        val edge1 = UndirectedEdge(vertex1, vertex2)
        val edge2 = UndirectedEdge(vertex1, vertex3)

        edge1.hashCode should not equal edge2.hashCode
      }









    // 3. WeightedDirectedEdge

    // Argument changes
    "An WeightedDirectedEdge" should "be equal to another edge with the same vertices and weight" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)

        edge1 shouldEqual edge2
    }

    "An WeightedDirectedEdge" should "be not equal to another edge with the same vertices but different weight" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 2)

        edge1 should not equal edge2
    }

    "An WeightedDirectedEdge" should "be equal to another edge with the inverse vertices and oposite direction" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex2, vertex1, Direction.Backward, 1)

        edge1 shouldEqual edge2
    }

    // With different Edge types

    "An WeightedDirectedEdge" should "be not equal to an DirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Forward)

        edge1 should not equal edge2
    }

    "An WeightedDirectedEdge" should "be not equal to an UndirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = UndirectedEdge(vertex1, vertex2)

        edge1 should not equal edge2
    }

    "An WeightedDirectedEdge" should "be not equal to an WeightedUndirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 1)

        edge1 should not equal edge2
    }

    // Test the JsonEncoder and JsonDecoder

    "An WeightedDirectedEdge JSON" should "be equal to another edge JSON with the same vertices and weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)

        edge1.toJson shouldEqual edge2.toJson
    }

    it should "be equal not equal to another edge JSON with the same vertices but different weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 2)

        edge1.toJson should not equal edge2.toJson
    }

    it should "be equal to another edge with the same vertices and weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = edge1.toJson.fromJson[WeightedDirectedEdge[String]].getOrElse(null)

        edge1 shouldEqual edge2
    }

    it should "be not equal to another edge with the same vertices but different weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 2)

        edge1 should not equal edge2
    }

    // Test the HashCode

    "An WeightedDirectedEdge HashCode" should "be consistent with equals" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)

        (edge1 == edge2) shouldEqual (edge1.hashCode == edge2.hashCode)
      }

      it should "differ for edges with different vertices or weight" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val vertex3 = Vertex(3, "data3")
        val edge1 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex3, Direction.Forward, 1)
        val edge3 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 2)

        edge1.hashCode should not equal edge2.hashCode
        edge1.hashCode should not equal edge3.hashCode
      }





    // 4. WeightedUndirectedEdge
    // Argument changes
    "An WeightedUndirectedEdge" should "be equal to another edge with the same vertices and weight" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 1)

        edge1 shouldEqual edge2
    }

    "An WeightedUndirectedEdge" should "be equal to another edge with the same vertices but in reverse order" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex2, vertex1, 1)

        edge1 shouldEqual edge2
    }

    "An WeightedUndirectedEdge" should "be not equal to another edge with different vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val vertex3 = Vertex(3)
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex3, 1)

        edge1 should not equal edge2
    }

    // With different Edge types

    "An WeightedUndirectedEdge" should "be not equal to an DirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = DirectedEdge(vertex1, vertex2, Direction.Forward)

        edge1 should not equal edge2
    }

    "An WeightedUndirectedEdge" should "be not equal to an UndirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = UndirectedEdge(vertex1, vertex2)

        edge1 should not equal edge2
    }

    "An WeightedUndirectedEdge" should "be not equal to an WeightedDirectedEdge with the same vertices" in {
        val vertex1 = Vertex(1)
        val vertex2 = Vertex(2)
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedDirectedEdge(vertex1, vertex2, Direction.Forward, 1)

        edge1 should not equal edge2
    }

    // Test the JsonEncoder and JsonDecoder

    "An WeightedUndirectedEdge JSON" should "be equal to another edge JSON with the same vertices and weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 1)

        edge1.toJson shouldEqual edge2.toJson
    }

    it should "be equal not equal to another edge JSON with the same vertices but in reverse order" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex2, vertex1, 1)

        edge1.toJson should not equal edge2.toJson
    }

    it should "be equal to another edge with the same vertices and weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = edge1.toJson.fromJson[WeightedUndirectedEdge[String]].getOrElse(null)

        edge1 shouldEqual edge2
    }

    it should "be not equal to another edge with the same vertices but different weight" in {
        val vertex1 = Vertex(1,"data1")
        val vertex2 = Vertex(2,"data2")
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 2)

        edge1 should not equal edge2
    }

    // Test the HashCode

    "An WeightedUndirectedEdge HashCode" should "be consistent with equals" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex2, 1)

        (edge1 == edge2) shouldEqual (edge1.hashCode == edge2.hashCode)
      }

      it should "differ for edges with different vertices or weight" in {
        val vertex1 = Vertex(1, "data1")
        val vertex2 = Vertex(2, "data2")
        val vertex3 = Vertex(3, "data3")
        val edge1 = WeightedUndirectedEdge(vertex1, vertex2, 1)
        val edge2 = WeightedUndirectedEdge(vertex1, vertex3, 1)
        val edge3 = WeightedUndirectedEdge(vertex1, vertex2, 2)

        edge1.hashCode should not equal edge2.hashCode
        edge1.hashCode should not equal edge3.hashCode
      }
    
    
}