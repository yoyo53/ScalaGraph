import com.scala.core._

import java.io.File
import zio.json._

val folder = "test-files/directed"
new File(s"$folder/json").listFiles.filter(_.isFile).map(_.getName).filter(_.endsWith(".json")).foreach(filename => {
    println(filename)
    val lines = io.Source.fromFile(s"$folder/json/$filename").getLines.toList.mkString("\n")
    val graph = lines.fromJson[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]())

    println("Json: " + (graph.toJson.fromJson[DirectedGraph[Int]].getOrElse(DirectedGraph[Int]()) == graph))
    println("GraphViz: " + (graph.serializeGraphViz == DirectedGraph.deserializeGraphViz[Int](graph.serializeGraphViz).serializeGraphViz))
})

val folder2 = "test-files/weighted-directed"
new File(s"$folder2/json").listFiles.filter(_.isFile).map(_.getName).filter(_.endsWith(".json")).foreach(filename => {
    println(filename)
    val lines = io.Source.fromFile(s"$folder2/json/$filename").getLines.toList.mkString("\n")
    val graph = lines.fromJson[WeightedDirectedGraph[Int]].getOrElse(WeightedDirectedGraph[Int]())

    println("Json: " + (graph.toJson.fromJson[WeightedDirectedGraph[Int]].getOrElse(WeightedDirectedGraph[Int]()) == graph))
    println("GraphViz: " + (graph.serializeGraphViz == WeightedDirectedGraph.deserializeGraphViz[Int](graph.serializeGraphViz).serializeGraphViz))
})
