import org.scalatest.tools.Runner

object MainSpec {
  def main(args: Array[String]): Unit = {
    val classes = List(
      "com.scala.core.VertexSpec",
      "com.scala.core.DirectionSpec",
      "com.scala.core.EdgeSpec",
      "com.scala.core.DirectedGraphSpec",
      "com.scala.core.WeightedDirectedGraphSpec",
      "com.scala.core.UndirectedGraphSpec",
      "com.scala.core.WeightedUndirectedGraphSpec",
    )

    Runner.run(classes.foldLeft(Array("-o", "-fW", "target/test-reports.txt"))((acc, c) => acc ++ Array("-s", c)))
  }
}
