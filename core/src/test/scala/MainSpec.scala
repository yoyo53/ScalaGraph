import org.scalatest.tools.Runner

object MainSpec {
  def main(args: Array[String]): Unit = {
    val classes = List(
      "com.scala.core.VertexSpec",
      "com.scala.core.DirectionSpec",
      "com.scala.core.EdgeSpec",
    )

    Runner.run(classes.foldLeft(Array("-o", "-fW", "target/test-reports.txt"))((acc, c) => acc ++ Array("-s", c)))
  }
}
