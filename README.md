# Scala Graph

## Project Overview

This project involves designing and implementing a generic graph data structure, various operations to modify and analyze the graphs and integrate those data structures in a ZIO application.

### Application Structure

The project is organized into several subprojects, each serving a specific purpose:

1. **Core Library (core)**: Contains the implementation of the generic graph data structure and operations.
2. **ZIO Application (ui)**: Integrates the core library into a ZIO HTTP application, providing state management and a CRUD API to interact with the application.

### Graph Data Structure

The graph data structure has been implemented using several case classes and traits. The implementation has also been done according to the functional programming principles, meaning all data types are completely immutables.

#### Vertex

The vertex is represented by a case class that includes a data attribute of a generic data type. This allows flexibility in storing different types of data within the graph.

#### Edge

The edges have been implemented slightly differently, as there are multiple types of edges to represent. Therefore, a generic edge trait has been implemented to define attributes shared by all edges, as well as several other traits extending this initial trait to represent the different other attributes an edge can possess (directed / undirected and weighted / unweighted).

Finally, 4 case classes each extend 2 of those mutually exclusive traits to represent all the different possible types of edges (unweighted directed, weighted undirected...).

#### Graph

The graph itself is defined with a structure very similar to the one of the edges. This means that there is a generic graph trait defining the common attributes, some other traits extending it to define the specific attributes about the type of the graph (weight and direction) and then the 4 case classes, one for each type of graph.

### Graph Operations

To be able to modify and analyze the graph data structure, a large number of methods have been implemented on the graphs traits and classes. Here is an overview of the implemented operations for the different types of graphs:

#### Common Operations

Most of the operations are common to all types of graph, and have therefore been defined directly in the generic graph trait. These includes:
- some methods to work on the sets of vertices and edges, like getters, setters, and methods for insertions and deletions.
- helper methods used to traverse the graph, like the one giving the neighbors of a given vertex
- methods to get the different matrices representing the graph (incidence, adjacency, degree...)
- methods for traversing or sorting the graph with BFS and DFS algorithms
- methods to find the shortest path between 2 vertices of the graph with 2 different algorithms (Floyd-Warshall and Dijkstra)

Some other methods, even if they are common to all graphs, have implementations that differ from one graph type to another. We can think of the obvious create method, but there is also for example the method to check for cycles which differ for directed and undirected graphs. Therefore, only the signature of those methods has been put in the generic graph trait while the implementation has been done in the lower level traits or classes.

#### Operations on Directed Graphs

Some methods specific to the directed graph has also been implemented, and have therefore been defined in the directed graph trait. Those methods include:
- getting the sources and sinks of the graph
- more analyze matrices like in and out degree
- a new method to get the rank of a vertex in the graph
- an additional topological sorting algorithm to traverse the graph

#### Operations on Weighted Directed Graphs

Finally, some specific operations for scheduling graphs have been implemented, which are only applicable on weighted directed graphs as a scheduling graph must by definition be both weighted and directed. Those methods are:
- a method to check if the graph is a scheduling graph, as being weighted and directed is not enough
- some more analyze operations on vertices to get their earliest date, latest date, total float and free float
- a method to get the critical paths of a graph

### Serialization and Deserialization

Several methods have been implemented to serialize and deserialize the data with generic data types to make it easier to export the graphs and save them to external files. The available serialization types are:

1. **JSON**: the different case classes for the vertices, edges and graph have all been configured with the ZIO JSON library to allow JSON serialization and deserialization.

2. **Graph Viz**: an extension has been developed allowing serialization and deserialization in the standard Graph Viz format. This extension is inspired from the ZIO JSON library, allowing the same operations on the graphs (toGraphViz, fromGraphViz and fromGraphVizPretty).

### ZIO 2 Application

To allow for a better handling of side effects, the core graph module has been integrated in a ZIO 2 application. The following design choices has been made for this application:

1. **ZIO HTTP API**: the application consists of a CRUD API built with the ZIO HTTP library. This API allows to perform all of the operations defined in the core module on the different types of graph.
2. **State Management**: A ZLayer service has been implemented to allow for persistent state management in the application across requests.
3. **Limitations**: 4 distinct base routes have been defined to work on the different types of graph, which have all been configured to work only with vertices of Integer data type. However, the routes libraries have been defined with integer data types so only a few modifications in the main application file are needed to change the data type.

### Unit Testing

Unit tests for each class and methods of the core module have been implemented. These unit tests have been made with the ScalaTest library by respecting the FlatSpec style when writing them.

## Usage Instructions

### Building the Application

To build the whole application, use the following `sbt` command:

```bash
sbt compile
```

### Testing the Application

To test the application, use the following command:

```bash
sbt test
```

### Running the Application

To run the ZIO HTTP application, use the following command:

```bash
sbt ui/run
```

An example scenario using the core graph module can be run with the following command:

```bash
sbt core/run
```

## Usage Examples

Here are a few examples of how to use the different modules of this project. Many graphs both in JSON and Graph Viz format are available in the test-files folder for testing purposes.

### Core Graph Module

The core module can be imported and used on its own as a complete graph managing library as follows.

#### Creation of Graph

```scala
val vertex1 = Vertex(1)
val vertex2 = Vertex(2)
val edge = DirectedEdge(vertex1, vertex2, Direction.Forward)
val graph = DirectedGraph[Int]()
   .addVertex(vertex1)
   .addVertex(vertex2)
   .addEdge(edge)
```

#### JSON Serialization and Deserialization

```scala
val jsonGraph = graph.toJson
val graph2 = jsonGraph.fromJson[DirectedGraph[Int]]
```

#### Graph Viz Serialization and Deserialization

```scala
val graphVizGraph = graph.toGraphViz
val graph3 = jsonGraph.fromGraphViz[DirectedGraph[Int]]
```

### ZIO Application

The same operations can also be done with the CRUD methods of the ZIO HTTP application with a JSON body as follows.

#### Create Empty Graph

```bash
curl -X PUT localhost:8080/directed
```

#### Add Vertex to Graph

```bash
curl -X PUT --data '{"id":1}' localhost:8080/directed/vertex
```

#### Get Current State

```bash
curl -X GET localhost:8080/directed
```

#### Delete Vertex from Graph

```bash
curl -X DELETE --data '{"id":1}' localhost:8080/directed/vertex
```

#### Clear Current State

```bash
curl -X DELETE localhost:8080/directed
```
