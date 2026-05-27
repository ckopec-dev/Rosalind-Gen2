# Rosalind Problem: Compute the Squared Error Distortion

## Problem Description
Given a set of data points and a set of centers, compute the squared error distortion, which is the sum of squared distances from each data point to its nearest center.

## Solution in Scala

```scala
import scala.io.Source
import scala.math.pow

object ComputeSquaredErrorDistortion {
  
  // Calculate Euclidean distance between two points
  def euclideanDistance(point1: Array[Double], point2: Array[Double]): Double = {
    math.sqrt(point1.zip(point2).map { case (x, y) => pow(x - y, 2) }.sum)
  }
  
  // Find the index of the nearest center to a point
  def nearestCenterIndex(point: Array[Double], centers: Array[Array[Double]]): Int = {
    val distances = centers.map(center => euclideanDistance(point, center))
    distances.zipWithIndex.minBy(_._1)._2
  }
  
  // Compute squared error distortion
  def computeSquaredErrorDistortion(points: Array[Array[Double]], 
                                   centers: Array[Array[Double]]): Double = {
    points.map { point =>
      val nearestCenterIndex = nearestCenterIndex(point, centers)
      val nearestCenter = centers(nearestCenterIndex)
      val distance = euclideanDistance(point, nearestCenter)
      pow(distance, 2)
    }.sum
  }
  
  def main(args: Array[String]): Unit = {
    // Read input from file (assuming standard input format)
    val lines = Source.stdin.getLines().toList
    
    // Parse points and centers
    val points = lines.drop(2).takeWhile(_.nonEmpty).map(_.split("\\s+").map(_.toDouble)).toArray
    val centers = lines.drop(2 + points.length).map(_.split("\\s+").map(_.toDouble)).toArray
    
    // Compute and print result
    val distortion = computeSquaredErrorDistortion(points, centers)
    println(f"$distortion%.3f")
  }
}
```

## Alternative Implementation with Better Input Parsing

```scala
import scala.io.Source
import scala.math.pow

object ComputeSquaredErrorDistortion {
  
  // Calculate Euclidean distance between two points
  def euclideanDistance(point1: Array[Double], point2: Array[Double]): Double = {
    math.sqrt(point1.zip(point2).map { case (x, y) => pow(x - y, 2) }.sum)
  }
  
  // Find the index of the nearest center to a point
  def nearestCenterIndex(point: Array[Double], centers: Array[Array[Double]]): Int = {
    centers.zipWithIndex.map { case (center, index) =>
      (euclideanDistance(point, center), index)
    }.minBy(_._1)._2
  }
  
  // Compute squared error distortion
  def computeSquaredErrorDistortion(points: Array[Array[Double]], 
                                   centers: Array[Array[Double]]): Double = {
    points.map { point =>
      val nearestCenterIndex = nearestCenterIndex(point, centers)
      val nearestCenter = centers(nearestCenterIndex)
      val distance = euclideanDistance(point, nearestCenter)
      pow(distance, 2)
    }.sum
  }
  
  def main(args: Array[String]): Unit = {
    val input = Source.stdin.getLines().toList
    val points = input.drop(2).takeWhile(_.nonEmpty).map(_.split("\\s+").map(_.toDouble)).toArray
    val centers = input.drop(2 + points.length).map(_.split("\\s+").map(_.toDouble)).toArray
    
    val distortion = computeSquaredErrorDistortion(points, centers)
    println(f"$distortion%.3f")
  }
}
```

## Example Usage

For input:
```
2
4
1.3 1.1
1.3 0.2
0.6 2.8
3.0 3.2
2.0 4.0
2.5 3.5
1.0 1.0
2.0 2.0
3.0 3.0
2.5 2.5
```

The program would:
1. Parse the points and centers
2. For each point, find the nearest center
3. Calculate the squared distance to that center
4. Sum all squared distances

## Key Features

- **Euclidean Distance Calculation**: Computes the standard Euclidean distance between points
- **Nearest Center Finding**: Uses zipWithIndex to efficiently find the closest center
- **Squared Error Distortion**: Sums up all squared distances from points to their nearest centers
- **Input Parsing**: Handles standard Rosalind input format with proper parsing
- **Precision**: Formats output to 3 decimal places as required

The time complexity is O(n × k × d) where n is the number of points, k is the number of centers, and d is the dimensionality of the points.

