import org.scalatest._
import com.scala.core._

class VertexSpec extends UnitSpec {
  "A Vertex" should "be instantiated with an id and optional data" in {
    val vertexWithoutData = Vertex(1)
    vertexWithoutData.id should be(1)
    vertexWithoutData.data should be(None)

    val vertexWithData = Vertex(2, Some("testData"))
    vertexWithData.id should be(2)
    vertexWithData.data should be(Some("testData"))
  }

  it should "be equal to another vertex with the same id and data" in {
    val vertex1 = Vertex(1, Some("data"))
    val vertex2 = Vertex(1, Some("data"))

    vertex1 shouldEqual vertex2
  }

  it should "not be equal to another vertex with a different id or data" in {
    val vertex1 = Vertex(1, Some("data"))
    val vertex2 = Vertex(2, Some("data"))
    val vertex3 = Vertex(1, Some("differentData"))

    vertex1 should not equal vertex2
    vertex1 should not equal vertex3
  }
}