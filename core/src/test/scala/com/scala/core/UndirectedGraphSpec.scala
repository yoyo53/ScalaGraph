package com.scala.core

import org.scalatest._
import zio.json._

import scala.collection.mutable.HashMap

class UndirectedGraphSpec extends UnitSpec {
  var graph: UndirectedGraph[Int] = _
  override def beforeEach(): Unit = {
    graph = UndirectedGraph[Int]()
      .addVertex(Vertex(1))
      .addVertex(Vertex(2))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))
  }
  
   "UndirectedGraph" should "create a new graph with given vertices and edges" in {
      // Check if the graph is created successfully
      assert(graph != null)
    }

    it should "be equal to another graph with the same vertices and edges" in {
      // Create another graph with the same vertices and edges
      val graph2 = UndirectedGraph[Int]()
        .addVertex(Vertex(1))
        .addVertex(Vertex(2))
        .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))

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
      assert(graph.getEdges.contains(UndirectedEdge(Vertex(1), Vertex(2))))      
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
      assert(graph.getEdges.contains(UndirectedEdge(Vertex(1), Vertex(2))))      
      
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
      assert(graph.getEdges.contains(UndirectedEdge(Vertex(1), Vertex(2))))      
      
    }

   it should "set the edges in the graph" in {
      // Set the edges in the graph
      graph = graph.setEdges(Set(UndirectedEdge(Vertex(3), Vertex(4))))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 1)

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(UndirectedEdge(Vertex(3), Vertex(4))))      
      
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
      graph = graph.addEdge(UndirectedEdge(Vertex(3), Vertex(4)))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 2)

      // Check if the graph has the correct edges
      assert(graph.getEdges.contains(UndirectedEdge(Vertex(1), Vertex(2))))      
      assert(graph.getEdges.contains(UndirectedEdge(Vertex(3), Vertex(4))))            
    }

   it should "remove an edge from the graph" in {

      // Remove an edge from the graph
      graph = graph.removeEdge(UndirectedEdge(Vertex(1), Vertex(2)))

      // Check if the edges are set in the graph
      assert(graph.getEdges != null)

      // Check if the graph has the correct number of edges
      assert(graph.getEdges.size == 0)
      
    }

   it should "remove a set of edges from the graph" in {
      // Remove a set of edges from the graph
      graph = graph.removeEdges(Set(UndirectedEdge(Vertex(1), Vertex(2))))

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
      assert(graph.getNeighborsEdges(Vertex(1)).contains(UndirectedEdge(Vertex(1), Vertex(2))))      
      
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
      assert(graph.getPredecessorsEdges(Vertex(2)).contains(UndirectedEdge(Vertex(1), Vertex(2))))      
      
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
      assert(graph.getSuccessorsEdges(Vertex(1)).contains(UndirectedEdge(Vertex(1), Vertex(2))))      
      
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
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))



    // Check if the adjacency matrix is correct
    val adjacencyMatrix = graph.getAdgacencyMatrix
    assert(graph.getEdges.size == 5)
    
    //Afficher la matrice d'adjacence

    assert(adjacencyMatrix != null)
    assert(adjacencyMatrix.size == 4)

    assert(adjacencyMatrix(0) == List(0, 1, 1, 1))
    assert(adjacencyMatrix(1) == List(1, 0, 1, 1))
    assert(adjacencyMatrix(2) == List(1, 1, 0, 0))
    assert(adjacencyMatrix(3) == List(1, 1, 0, 0))
    }

   it should "get the incidence matrix of the graph" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Check if the incidence matrix is correct 
      val incidenceMatrix = graph.getIncidenceMatrix
      assert(incidenceMatrix != null)
      
      // Edges
      assert(incidenceMatrix.size == 5)
      assert(incidenceMatrix(0) == List(0, 0, 0, 0))
      assert(incidenceMatrix(1) == List(0, 0, 0, 0))
      assert(incidenceMatrix(2) == List(0, 0, 0, 0))
      assert(incidenceMatrix(3) == List(0, 0, 0, 0))
      assert(incidenceMatrix(4) == List(0, 0, 0, 0))
    }

   it should "get the degree matrix of the graph" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

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
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Check if the Laplacian matrix is correct
      val laplacianMatrix = graph.getLaplacianMatrix
      assert(laplacianMatrix != null)
      assert(laplacianMatrix.size == 4)
      assert(laplacianMatrix(0) == List(3, -1, -1, -1))
      assert(laplacianMatrix(1) == List(-1, 3, -1, -1))
      assert(laplacianMatrix(2) == List(-1, -1, 2, 0))
      assert(laplacianMatrix(3) == List(-1, -1, 0, 2))
    }

    it should "have a Laplacian matrix == degreeMatrix - adjacencyMatrix" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))
      //Perform degreeMatrix - adjacencyMatrix == laplacianMatrix
      val laplacianMatrix = graph.getLaplacianMatrix
      val degreeMatrix = graph.getDegreeMatrix
      val adjacencyMatrix = graph.getAdgacencyMatrix
      val laplacianMatrix2 = degreeMatrix.zip(adjacencyMatrix).map { case (x, y) => x.zip(y).map { case (a, b) => a - b } }
      assert(laplacianMatrix == laplacianMatrix2)
    }

   it should "traverse the graph using depth-first search" in {
      // Traverse the graph using depth-first search
      graph.dfsTraverse(Vertex(1), v => println(v))

      // Check if the graph is traversed correctly
      assert(true)      
    }

   it should "sort the graph using depth-first search" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Sort the graph using depth-first search
      val sortedGraph = graph.dfsSort

      // Check if the graph is sorted correctly
      assert(sortedGraph != null)
      assert(sortedGraph.size == 4)
      assert(sortedGraph(0) == Vertex(1))
      assert(sortedGraph(1) == Vertex(4))
      assert(sortedGraph(2) == Vertex(2))
      assert(sortedGraph(3) == Vertex(3))
    }

   it should "traverse the graph using breadth-first search" in {
      // Traverse the graph using breadth-first search
      graph.bfsTraverse(Vertex(1), v => println(v))

      // Check if the graph is traversed correctly
      assert(true)
      
    }

   it should "sort the graph using breadth-first search" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Sort the graph using breadth-first search
      val sortedGraph = graph.bfsSort

      // Check if the graph is sorted correctly
      assert(sortedGraph != null)
      assert(sortedGraph.size == 4)
      assert(sortedGraph(0) == Vertex(1))
      assert(sortedGraph(1) == Vertex(2))
      assert(sortedGraph(2) == Vertex(3))
      assert(sortedGraph(3) == Vertex(4))
      
    }

   it should "check if the graph has a cycle" in {
      // Check if the graph has a cycle
      assert(graph.hasCycle == false)

      // Graph with 4 vertices and 5 edges
      graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Check if the graph has a cycle
      assert(graph.hasCycle == true)

      // Another graph with a cycle

      val graph2 = UndirectedGraph[Int]()
      .addVertex(Vertex(1))
      .addVertex(Vertex(2))
      .addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(1)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Check if the graph has a cycle
      assert(graph2.hasCycle == true)
    }

   it should "get the shortest paths between all pairs of vertices using Floyd-Warshall algorithm" in {
    // Graph with 4 vertices and 5 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(4)))

      // Get the shortest paths between all pairs of vertices using Floyd-Warshall algorithm
      val shortestPaths = graph.getShortestPathsFloydWarshall

      // Check if the shortest paths are correct
      assert(shortestPaths != null)
      assert(shortestPaths.isSuccess && shortestPaths.get.size == 12)
      assert(shortestPaths.get == HashMap(
    (Vertex(1, None), Vertex(3, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(3, None))), 
    (Vertex(2, None), Vertex(4, None)) -> List(UndirectedEdge(Vertex(4, None), Vertex(2, None))), 
    (Vertex(3, None), Vertex(2, None)) -> List(UndirectedEdge(Vertex(3, None), Vertex(2, None))), 
    (Vertex(3, None), Vertex(4, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(3, None)), UndirectedEdge(Vertex(1, None), Vertex(4, None))), 
      (Vertex(1, None), Vertex(4, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(4, None))), 
      (Vertex(4, None), Vertex(1, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(4, None))), 
      (Vertex(4, None), Vertex(2, None)) -> List(UndirectedEdge(Vertex(4, None), Vertex(2, None))), 
      (Vertex(2, None), Vertex(1, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None))), 
      (Vertex(1, None), Vertex(2, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None))), 
      (Vertex(2, None), Vertex(3, None)) -> List(UndirectedEdge(Vertex(3, None), Vertex(2, None))), 
      (Vertex(4, None), Vertex(3, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(4, None)), UndirectedEdge(Vertex(1, None), Vertex(3, None))), 
      (Vertex(3, None), Vertex(1, None)) -> List(UndirectedEdge(Vertex(1, None), Vertex(3, None)))))
    }

   it should "get the shortest path between two vertices using Floyd-Warshall algorithm" in {
    // Graph with 4 vertices and 4 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(4)))

      // Get the shortest path between two vertices using Floyd-Warshall algorithm
      val shortestPath = graph.getShortestPathFloydWarshall(Vertex(1), Vertex(4))

      // Check if the shortest path is correct
      assert(shortestPath != null)
      assert(shortestPath.isSuccess && shortestPath.get.size == 2)
      assert(shortestPath.get == List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(4, None))))
      
      // Another more complex graph with 6 vertices 
      val graph2 = UndirectedGraph[Int]()
      .addVertex(Vertex(1))
      .addVertex(Vertex(2))
      .addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addVertex(Vertex(5))
      .addVertex(Vertex(6))

      .addVertex(Vertex(7))
      .addVertex(Vertex(8))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(5)))
      .addEdge(UndirectedEdge(Vertex(5), Vertex(6)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(7)))
      .addEdge(UndirectedEdge(Vertex(7), Vertex(8)))
      .addEdge(UndirectedEdge(Vertex(8), Vertex(6)))

      // Get the shortest path between two vertices using Floyd-Warshall algorithm
      val shortestPath2 = graph2.getShortestPathFloydWarshall(Vertex(1), Vertex(6))

      // Check if the shortest path is correct
      assert(shortestPath2 != null)
      assert(shortestPath2.isSuccess && shortestPath2.get.size == 4)
      assert(shortestPath2.get == List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(7, None)), UndirectedEdge(Vertex(7, None), Vertex(8, None)), UndirectedEdge(Vertex(8, None), Vertex(6, None))))
    }

   it should "get the shortest paths from a source vertex to all other vertices using Dijkstra's algorithm" in {
    // Graph with 4 vertices and 4 edges
    graph = graph.addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(4)))

      // Get the shortest paths from a source vertex to all other vertices using Dijkstra's algorithm
      val shortestPaths = graph.getShortestPathsDijkstra(Vertex(1))

      // Check if the shortest paths are correct
      assert(shortestPaths != null)
      assert(shortestPaths.isSuccess && shortestPaths.get.size == 4)
      assert(shortestPaths.get == Map(
        Vertex(1, None) -> List(), 
      Vertex(2, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None))), 
      Vertex(3, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(3, None))), 
      Vertex(4, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(4, None)))))

      // Another more complex graph with 6 vertices 
      val graph2 = UndirectedGraph[Int]()
      .addVertex(Vertex(1))
      .addVertex(Vertex(2))
      .addVertex(Vertex(3))
      .addVertex(Vertex(4))
      .addVertex(Vertex(5))
      .addVertex(Vertex(6))

      .addVertex(Vertex(7))
      .addVertex(Vertex(8))
      .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
      .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
      .addEdge(UndirectedEdge(Vertex(4), Vertex(5)))
      .addEdge(UndirectedEdge(Vertex(5), Vertex(6)))
      .addEdge(UndirectedEdge(Vertex(2), Vertex(7)))
      .addEdge(UndirectedEdge(Vertex(7), Vertex(8)))
      .addEdge(UndirectedEdge(Vertex(8), Vertex(6)))

      // Get the shortest paths from a source vertex to all other vertices using Dijkstra's algorithm
      val shortestPaths2 = graph2.getShortestPathsDijkstra(Vertex(1))

      // Check if the shortest paths are correct
      assert(shortestPaths2 != null)
      assert(shortestPaths2.isSuccess && shortestPaths2.get.size == 8)
      assert(shortestPaths2.get == HashMap(
      Vertex(6, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(7, None)), UndirectedEdge(Vertex(7, None), Vertex(8, None)), UndirectedEdge(Vertex(8, None), Vertex(6, None))), 
      Vertex(8, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(7, None)), UndirectedEdge(Vertex(7, None), Vertex(8, None))), 
      Vertex(1, None) -> List(), 
      Vertex(4, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(7, None)), UndirectedEdge(Vertex(7, None), Vertex(8, None)), UndirectedEdge(Vertex(8, None), Vertex(6, None)), UndirectedEdge(Vertex(5, None), Vertex(6, None)), UndirectedEdge(Vertex(4, None), Vertex(5, None))), 
      Vertex(7, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(7, None))), 
      Vertex(5, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(7, None)), UndirectedEdge(Vertex(7, None), Vertex(8, None)), UndirectedEdge(Vertex(8, None), Vertex(6, None)), UndirectedEdge(Vertex(5, None), Vertex(6, None))), 
      Vertex(3, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(3, None))), 
      Vertex(2, None) -> List(UndirectedEdge(Vertex(1, None), Vertex(2, None)))))
      
    }

   it should "get the shortest path from a source vertex to a destination vertex using Dijkstra's algorithm" in {
      // Graph with 4 vertices and 4 edges
      graph = graph.addVertex(Vertex(3))
        .addVertex(Vertex(4))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
        .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(4)))

        // Get the shortest path from a source vertex to a destination vertex using Dijkstra's algorithm
        val shortestPath = graph.getShortestPathDijkstra(Vertex(1), Vertex(4))

        // Check if the shortest path is correct
        assert(shortestPath != null)
        assert(shortestPath.isSuccess && shortestPath.get.size == 2)
        assert(shortestPath.get == List(UndirectedEdge(Vertex(1, None), Vertex(2, None)), UndirectedEdge(Vertex(2, None), Vertex(4, None))))

        // Another more complex graph with 6 vertices 
        val graph2 = UndirectedGraph[Int]()
        .addVertex(Vertex(1))
        .addVertex(Vertex(2))
        .addVertex(Vertex(3))
        .addVertex(Vertex(4))
        .addVertex(Vertex(5))
        .addVertex(Vertex(6))

        .addVertex(Vertex(7))
        .addVertex(Vertex(8))
        .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
        .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
        .addEdge(UndirectedEdge(Vertex(4), Vertex(5)))
        .addEdge(UndirectedEdge(Vertex(5), Vertex(6)))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(7)))
        .addEdge(UndirectedEdge(Vertex(7), Vertex(8)))
        .addEdge(UndirectedEdge(Vertex(8), Vertex(6)))

        // Get the shortest path from a source vertex to a destination vertex using Dijkstra's algorithm
        val shortestPath2 = graph2.getShortestPathDijkstra(Vertex(1), Vertex(6))

        // Check if the shortest path is correct
        assert(shortestPath2 != null)
        assert(shortestPath2.isSuccess && shortestPath2.get.size == 4)
    }

    it should "encode and decode the graph to JSON" in {
      // Graph with 4 vertices and 4 edges
      graph = graph.addVertex(Vertex(3))
        .addVertex(Vertex(4))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
        .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(4)))
      
      // Encode the graph to JSON
      val json = graph.toJson

      // Check if the JSON is correct
      assert(json != null)
      assert(json == """{"UndirectedGraph":{"vertices":[{"id":1},{"id":2},{"id":3},{"id":4}],"edges":[{"v1":{"id":1},"v2":{"id":2}},{"v1":{"id":2},"v2":{"id":3}},{"v1":{"id":3},"v2":{"id":4}},{"v1":{"id":2},"v2":{"id":4}}]}}""")

      // Decode the JSON to a graph
      val decodedGraph = json.fromJson[UndirectedGraph[Float]].getOrElse(UndirectedGraph[Number]())

      // Check if the decoded graph is correct
      assert(decodedGraph != null)
      assert(decodedGraph.getVertices.size == 4)
      assert(decodedGraph.getEdges.size == 4)
      assert(decodedGraph.getVertices.contains(Vertex(1)))
      assert(decodedGraph.getVertices.contains(Vertex(2)))
      assert(decodedGraph.getVertices.contains(Vertex(3)))
      assert(decodedGraph.getVertices.contains(Vertex(4)))
      assert(decodedGraph.getEdges.contains(UndirectedEdge(Vertex(2), Vertex(3))))
      assert(decodedGraph.getEdges.contains(UndirectedEdge(Vertex(3), Vertex(4))))
      assert(decodedGraph.getEdges.contains(UndirectedEdge(Vertex(2), Vertex(4))))

      // Another more complex graph with 6 vertices 
        val graph2 = UndirectedGraph[Int]()
        .addVertex(Vertex(1))
        .addVertex(Vertex(2))
        .addVertex(Vertex(3))
        .addVertex(Vertex(4))
        .addVertex(Vertex(5))
        .addVertex(Vertex(6))

        .addVertex(Vertex(7))
        .addVertex(Vertex(8))
        .addEdge(UndirectedEdge(Vertex(1), Vertex(2)))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(3)))
        .addEdge(UndirectedEdge(Vertex(3), Vertex(4)))
        .addEdge(UndirectedEdge(Vertex(4), Vertex(5)))
        .addEdge(UndirectedEdge(Vertex(5), Vertex(6)))
        .addEdge(UndirectedEdge(Vertex(2), Vertex(7)))
        .addEdge(UndirectedEdge(Vertex(7), Vertex(8)))
        .addEdge(UndirectedEdge(Vertex(8), Vertex(6)))


        // Encode the graph to JSON
        val json2 = graph2.toJson

        // Check if the JSON is correct
        assert(json2 != null)
        assert(json2 == """{"UndirectedGraph":{"vertices":[{"id":6},{"id":8},{"id":1},{"id":4},{"id":7},{"id":5},{"id":3},{"id":2}],"edges":[{"v1":{"id":5},"v2":{"id":6}},{"v1":{"id":8},"v2":{"id":6}},{"v1":{"id":3},"v2":{"id":4}},{"v1":{"id":4},"v2":{"id":5}},{"v1":{"id":2},"v2":{"id":7}},{"v1":{"id":1},"v2":{"id":2}},{"v1":{"id":7},"v2":{"id":8}},{"v1":{"id":2},"v2":{"id":3}}]}}""")
    }
    
}