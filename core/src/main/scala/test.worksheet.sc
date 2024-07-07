import com.scala.core._

import java.io.File
import zio.json._

val folder = "test-files/directed"
new File(s"$folder/json").listFiles.filter(_.isFile).map(_.getName).filter(_.endsWith(".json")).foreach(filename => {
    println(filename)
    val lines = io.Source.fromFile(s"$folder/json/$filename").getLines.toList.mkString("\n")
    val graph = lines.fromJson[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]())

    println("Json: " + (graph.toJson.fromJson[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]()) == graph))
    println("GraphViz: " + (graph.toGraphViz.fromGraphViz[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]()) == graph))
})

val folder2 = "test-files/weighted-directed"
new File(s"$folder2/json").listFiles.filter(_.isFile).map(_.getName).filter(_.endsWith(".json")).foreach(filename => {
    println(filename)
    val lines = io.Source.fromFile(s"$folder2/json/$filename").getLines.toList.mkString("\n")
    val graph = lines.fromJson[WeightedDirectedGraph[Int]].getOrElse(WeightedDirectedGraph[Int]())

    println("Json: " + (graph.toJson.fromJson[WeightedDirectedGraph[Int]].getOrElse(WeightedDirectedGraph[Int]()) == graph))
    println("GraphViz: " + (graph.toGraphViz.fromGraphViz[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]()) == graph))
})

io.Source.fromFile("test-files/directed/json/graph-25.json")
    .mkString
    .fromJson[DirectedGraph[Int]]
    .getOrElse(null)
    .toJson
    .fromJson[DirectedGraph[Int]]
    .getOrElse(null)
    .toJsonPretty
    .fromJson[DirectedGraph[Int]]
    .getOrElse(null)
    .toGraphViz
    .fromGraphViz[DirectedGraph[Int]]
    .getOrElse(null)
    .toGraphVizPretty
    .fromGraphViz[DirectedGraph[Int]]
    .getOrElse(null)
