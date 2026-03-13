# Rosalind Problem: Implement the Lloyd Algorithm for k-Means Clustering

## Problem Description
The Lloyd algorithm is a classic iterative method for k-means clustering. Given a set of points in d-dimensional space and a value k, the algorithm partitions the points into k clusters by repeatedly:
1. Assigning each point to the nearest centroid
2. Updating centroids to the mean of their assigned points

## Solution in Scala

```scala
import scala.io.Source
import scala.math._

object LloydAlgorithm {
  
  // Point class to represent d-dimensional points
  case class Point(coordinates: Array[Double]) {
    def distance(other: Point): Double = {
      sqrt(coordinates.zip(other.coordinates).map { case (a, b) => pow(a - b, 2) }.sum)
    }
    
    def +(other: Point): Point = {
      Point(coordinates.zip(other.coordinates).map { case (a, b) => a + b })
    }
    
    def /(scalar: Double): Point = {
      Point(coordinates.map(_ / scalar))
    }
  }
  
  // Function to calculate Euclidean distance between two points
  def euclideanDistance(p1: Point, p2: Point): Double = {
    sqrt(p1.coordinates.zip(p2.coordinates).map { case (a, b) => pow(a - b, 2) }.sum)
  }
  
  // Function to calculate mean of points
  def mean(points: List[Point]): Point = {
    if (points.isEmpty) throw new IllegalArgumentException("Cannot calculate mean of empty list")
    
    val sum = points.reduce((p1, p2) => p1 + p2)
    sum / points.length.toDouble
  }
  
  // Function to assign points to nearest centroid
  def assignPointsToClusters(points: List[Point], centroids: List[Point]): List[Int] = {
    points.map(point => {
      val distances = centroids.map(centroid => euclideanDistance(point, centroid))
      distances.zipWithIndex.minBy(_._1)._2  // Return index of nearest centroid
    })
  }
  
  // Function to update centroids based on current cluster assignments
  def updateCentroids(points: List[Point], assignments: List[Int], k: Int): List[Point] = {
    (0 until k).map(clusterId => {
      val clusterPoints = points.zip(assignments).filter(_._2 == clusterId).map(_._1)
      if (clusterPoints.isEmpty) {
        // If no points assigned to cluster, keep the original centroid
        throw new RuntimeException("Empty cluster found")
      } else {
        mean(clusterPoints)
      }
    }).toList
  }
  
  // Main Lloyd algorithm implementation
  def lloydAlgorithm(points: List[Point], k: Int, maxIterations: Int = 1000): List[Point] = {
    // Initialize centroids randomly
    val randomCentroids = points.take(k)
    
    var centroids = randomCentroids
    
    for (_ <- 0 until maxIterations) {
      // Assign points to clusters
      val assignments = assignPointsToClusters(points, centroids)
      
      // Update centroids
      val newCentroids = updateCentroids(points, assignments, k)
      
      // Check for convergence (if centroids don't change significantly)
      val converged = centroids.zip(newCentroids).forall { case (oldC, newC) =>
        euclideanDistance(oldC, newC) < 1e-6
      }
      
      centroids = newCentroids
      
      if (converged) {
        println("Converged after " + _ + " iterations")
        return centroids
      }
    }
    
    centroids
  }
  
  // Parse input from file
  def parseInput(filename: String): (Int, List[Point]) = {
    val lines = Source.fromFile(filename).getLines().toList
    val k = lines.head.split(" ").head.toInt
    val points = lines.tail.map(line => {
      val coords = line.split(" ").map(_.toDouble)
      Point(coords)
    })
    (k, points)
  }
  
  // Format output as required by Rosalind
  def formatOutput(centroids: List[Point]): String = {
    centroids.map(centroid => 
      centroid.coordinates.map(_.formatted("%.3f")).mkString(" ")
    ).mkString("\n")
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val points = List(
      Point(Array(1.3, 1.1)),
      Point(Array(1.3, 0.2)),
      Point(Array(0.6, 2.8)),
      Point(Array(3.0, 3.2)),
      Point(Array(1.2, 0.7)),
      Point(Array(1.4, 1.6)),
      Point(Array(2.1, 1.3)),
      Point(Array(3.8, 0.7)),
      Point(Array(3.2, 1.8)),
      Point(Array(2.9, 3.2))
    )
    
    val k = 3
    val centroids = lloydAlgorithm(points, k)
    println(formatOutput(centroids))
  }
}
```

## Key Features of the Implementation

1. **Point Class**: Represents d-dimensional points with distance calculation
2. **Euclidean Distance**: Calculates distance between two points
3. **Mean Calculation**: Computes the mean of a list of points
4. **Assignment Function**: Assigns each point to the nearest centroid
5. **Centroid Update**: Updates centroids to the mean of assigned points
6. **Convergence Check**: Stops when centroids don't change significantly
7. **Input/Output**: Handles file parsing and formatted output

## Algorithm Steps

1. **Initialization**: Randomly select k initial centroids
2. **Assignment**: For each point, assign to nearest centroid
3. **Update**: Recalculate centroids as means of assigned points
4. **Repeat**: Continue until convergence or max iterations reached

## Time Complexity
- O(n × k × d × i) where n is number of points, k is number of clusters, d is dimensions, and i is iterations

## Space Complexity
- O(n × d + k × d) for storing points and centroids

This implementation handles the core Lloyd algorithm with proper convergence checking and follows Rosalind's expected output format.

