import org.scalatest._

class VertexSpec extends UnitSpec {
    var vertex: Vertex = _

    before {
        vertex = new Vertex(new Graph, "A")
    }

    "Vertex" should "be equal to another vertex with the same name" in {
        val other = new Vertex(new Graph, "B")
        vertex should equal(other)
    }
}