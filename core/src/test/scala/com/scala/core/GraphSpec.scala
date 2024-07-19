package com.scala.core

import org.scalatest._
import zio.json._

import org.scalatest.BeforeAndAfterEach

class GraphSpec extends UnitSpec with BeforeAndAfterEach {
  var graph: DirectedGraph[Int] = _
  override def beforeEach(): Unit = {
    graph = DirectedGraph[Int]()
      .addVertex(Vertex(1))
      .addVertex(Vertex(2))
      .addEdge(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward))
  }
  
   "DirectedGraph" should "create a new graph with given vertices and edges" in {
      // Check if the graph is created successfully
      assert(graph != null)
    }

    it should "be equal to another graph with the same vertices and edges" in {
      // Create another graph with the same vertices and edges
      val graph2 = DirectedGraph[Int]()
        .addVertex(Vertex(1))
        .addVertex(Vertex(2))
        .addEdge(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward))

      // Check if the two graphs are equal
      assert(graph == graph2)
    }

   it should "get the set of vertices in the graph" in {
      // Check if the vertices are set in the graph
      assert(graph.getVertices != null)

      // Check if the graph has the correct number of vertices
      assert(graph.getVertices.size == 2)
      assert(graph.getEdges.size == 1)

      // Check if the graph has the correct vertices
      assert(graph.getVertices.contains(Vertex(1)))
      assert(graph.getVertices.contains(Vertex(2)))

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
    }

   it should "set the vertices in the graph" in {
      // Set the vertices in the graph
      graph = graph.setVertices(Set(Vertex(3), Vertex(4)))

      // Check if the vertices are set in the graph
      assert(graph.getVertices != null)

      // Check if the graph has the correct number of vertices
      assert(graph.getVertices.size == 2)
      assert(graph.getEdges.size == 0)

      // Check if the graph has the correct vertices
      assert(graph.getVertices.contains(Vertex(3)))
      assert(graph.getVertices.contains(Vertex(4)))
      
    }

   it should "clear all vertices in the graph" in {
      // Clear all vertices in the graph
      graph = graph.clearVertices

      // Check if the vertices are cleared in the graph
      assert(graph.getVertices != null)

      // Check if the graph has the correct number of vertices
      assert(graph.getVertices.size == 0)
      
    }

   it should "add a vertex to the graph" in {
      // Add a vertex to the graph
      graph = graph.addVertex(Vertex(3))

      // Check if the vertices are set in the graph
      assert(graph.getVertices != null)

      // Check if the graph has the correct number of vertices
      assert(graph.getVertices.size == 3)
      assert(graph.getEdges.size == 1)

      // Check if the graph has the correct vertices
      assert(graph.getVertices.contains(Vertex(1)))
      assert(graph.getVertices.contains(Vertex(2)))
      assert(graph.getVertices.contains(Vertex(3)))

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
      
    }

   it should "remove a vertex from the graph" in {
      // Remove a vertex from the graph
      graph = graph.removeVertex(Vertex(2))

      // Check if the vertices are set in the graph
      assert(graph.getVertices != null)

      // Check if the graph has the correct number of vertices
      assert(graph.getVertices.size == 1)
      assert(graph.getEdges.size == 0)

      // Check if the graph has the correct vertices
      assert(graph.getVertices.contains(Vertex(1)))
      assert(!graph.getVertices.contains(Vertex(2)))
      
    }

   it should "get the set of edges in the graph" in {
      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 1)

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
      
    }

   it should "set the edges in the graph" in {
      // Set the edges in the graph
      graph = graph.setEdges(Set(DirectedEdge(Vertex(3), Vertex(4), Direction.Forward)))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 1)

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(DirectedEdge(Vertex(3), Vertex(4), Direction.Forward)))      
      
    }

   it should "clear all edges in the graph" in {

      // Clear all edges in the graph
      graph = graph.clearEdges

      // Check if the edges are cleared in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 0)
      
    }

   it should "add an edge to the graph" in {
      // Add an edge to the graph
      graph = graph.addEdge(DirectedEdge(Vertex(3), Vertex(4), Direction.Forward))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 2)

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
      assert(graph.getEdges.contains(DirectedEdge(Vertex(3), Vertex(4), Direction.Forward)))            
    }

   it should "remove an edge from the graph" in {

      // Remove an edge from the graph
      graph = graph.removeEdge(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 0)
      
    }

   it should "remove a set of edges from the graph" in {
      // Remove a set of edges from the graph
      graph = graph.removeEdges(Set(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 0)
      
    }

   it should "get the neighbors edges of a vertex" in {
      // Check if the neighbors edges are set in the graph
      assert(graph.getNeighborsEdges(Vertex(1)) != null)

      // Check if the graph has the correct number of neighbors edges
      assert(graph.getNeighborsEdges(Vertex(1)).size == 1)

      // Check if the graph has the correct neighbors edges
      assert(graph.getNeighborsEdges(Vertex(1)).contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
      
    }

   it should "get the neighbors of a vertex" in {
      // Check if the neighbors are set in the graph
      assert(graph.getNeighbors(Vertex(1)) != null)

      // Check if the graph has the correct number of neighbors
      assert(graph.getNeighbors(Vertex(1)).size == 1)

      // Check if the graph has the correct neighbors
      assert(graph.getNeighbors(Vertex(1)).contains(Vertex(2)))      
      
    }

   it should "get the predecessors edges of a vertex" in {
      // Check if the predecessors edges are set in the graph
      assert(graph.getPredecessorsEdges(Vertex(2)) != null)

      // Check if the graph has the correct number of predecessors edges
      assert(graph.getPredecessorsEdges(Vertex(2)).size == 1)

      // Check if the graph has the correct predecessors edges
      assert(graph.getPredecessorsEdges(Vertex(2)).contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
      
    }

   it should "get the predecessors of a vertex" in {
      // Check if the predecessors are set in the graph
      assert(graph.getPredecessors(Vertex(2)) != null)

      // Check if the graph has the correct number of predecessors
      assert(graph.getPredecessors(Vertex(2)).size == 1)

      // Check if the graph has the correct predecessors
      assert(graph.getPredecessors(Vertex(2)).contains(Vertex(1)))      
      
    }

   it should "get the successors edges of a vertex" in {
      // Check if the successors edges are set in the graph
      assert(graph.getSuccessorsEdges(Vertex(1)) != null)

      // Check if the graph has the correct number of successors edges
      assert(graph.getSuccessorsEdges(Vertex(1)).size == 1)

      // Check if the graph has the correct successors edges
      assert(graph.getSuccessorsEdges(Vertex(1)).contains(DirectedEdge(Vertex(1), Vertex(2), Direction.Forward)))      
      
    }

   it should "get the successors of a vertex" in {
      // Check if the successors are set in the graph
      assert(graph.getSuccessors(Vertex(1)) != null)

      // Check if the graph has the correct number of successors
      assert(graph.getSuccessors(Vertex(1)).size == 1)

      // Check if the graph has the correct successors
      assert(graph.getSuccessors(Vertex(1)).contains(Vertex(2)))
      
    }

   it should "get the degree of a vertex" in {
      // Check if the degree of the vertex is correct
      assert(graph.getDegree(Vertex(1)) == 1)
      assert(graph.getDegree(Vertex(2)) == 1)
      
    }

   it should "get the adjacency matrix of the graph" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(3), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(4), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(1), Vertex(4), Direction.Forward))



    // Check if the adjacency matrix is correct
    val adjacencyMatrix = graph.getAdgacencyMatrix
    assert(graph.getEdges.size == 5)
    
    //Afficher la matrice d'adjacence

    assert(adjacencyMatrix != null)
    assert(adjacencyMatrix.size == 4)

    assert(adjacencyMatrix(0) == List(0, 1, 1, 1))
    assert(adjacencyMatrix(1) == List(0, 0, 0, 0))
    assert(adjacencyMatrix(2) == List(0, 1, 0, 0))
    assert(adjacencyMatrix(3) == List(0, 1, 0, 0))
    }

   it should "get the incidence matrix of the graph" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(3), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(4), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(1), Vertex(4), Direction.Forward))

      // Check if the incidence matrix is correct 
      val incidenceMatrix = graph.getIncidenceMatrix
      assert(incidenceMatrix != null)
      
      // Edges
      assert(incidenceMatrix.size == 5)
      assert(incidenceMatrix(0) == List(0, -1, 1, 0)) //List(1, -1, 0, 0)
      assert(incidenceMatrix(1) == List(0, -1, 0, 1)) //List(1, 0, -1, 0)
      assert(incidenceMatrix(2) == List(1, 0, -1, 0)) //List(0, -1, 1, 0)
      assert(incidenceMatrix(3) == List(1, 0, 0, -1)) //List(0, -1, 0, 1)
      assert(incidenceMatrix(4) == List(1, -1, 0, 0)) //List(1, 0, 0, -1)
    }

   it should "get the degree matrix of the graph" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(3), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(4), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(1), Vertex(4), Direction.Forward))

      // Check if the degree matrix is correct
      val degreeMatrix = graph.getDegreeMatrix
      assert(degreeMatrix != null)
      assert(degreeMatrix.size == 4)
      assert(degreeMatrix(0) == List(3, 0, 0, 0))
      assert(degreeMatrix(1) == List(0, 3, 0, 0))
      assert(degreeMatrix(2) == List(0, 0, 2, 0))
      assert(degreeMatrix(3) == List(0, 0, 0, 2))
      
    }

   it should "get the Laplacian matrix of the graph" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(3), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(4), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(1), Vertex(4), Direction.Forward))

      // Check if the Laplacian matrix is correct
      val laplacianMatrix = graph.getLaplacianMatrix
      assert(laplacianMatrix != null)
      assert(laplacianMatrix.size == 4)
      assert(laplacianMatrix(0) == List(3, -1, -1, -1))
      assert(laplacianMatrix(1) == List(0, 3, 0, 0))
      assert(laplacianMatrix(2) == List(0, -1, 2, 0))
      assert(laplacianMatrix(3) == List(0, -1, 0, 2))
    }

    it should "have a Laplacian matrix == degreeMatrix - adjacencyMatrix" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(DirectedEdge(Vertex(1), Vertex(3), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(3), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(4), Vertex(2), Direction.Forward))
      .addEdge(DirectedEdge(Vertex(1), Vertex(4), Direction.Forward))
      //Perform degreeMatrix - adjacencyMatrix == laplacianMatrix
      val laplacianMatrix = graph.getLaplacianMatrix
      val degreeMatrix = graph.getDegreeMatrix
      val adjacencyMatrix = graph.getAdgacencyMatrix
      val laplacianMatrix2 = degreeMatrix.zip(adjacencyMatrix).map { case (x, y) => x.zip(y).map { case (a, b) => a - b } }
      assert(laplacianMatrix == laplacianMatrix2)
    }

   it should "traverse the graph using depth-first search" in {

      
    }

   it should "sort the graph using depth-first search" in {
      
    }

   it should "traverse the graph using breadth-first search" in {
      
    }

   it should "sort the graph using breadth-first search" in {
      
    }

   it should "check if the graph has a cycle" in {
      
    }

   it should "get the shortest paths between all pairs of vertices using Floyd-Warshall algorithm" in {
      
    }

   it should "get the shortest path between two vertices using Floyd-Warshall algorithm" in {
      
    }

   it should "get the shortest paths from a source vertex to all other vertices using Dijkstra's algorithm" in {
      
    }

   it should "get the shortest path from a source vertex to a destination vertex using Dijkstra's algorithm" in {
      
    }

    it should "encode and decode the graph to JSON" in {

    }
    
}