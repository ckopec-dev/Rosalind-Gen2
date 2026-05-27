# Rosalind Problem: Construct the Graph of a Spectrum

## Problem Description

Given a collection of integers representing a spectrum, construct the graph of the spectrum where vertices are the integers and edges represent the differences between adjacent vertices.

## Solution

```scala
object ConstructTheGraphOfASpectrum {
  
  def constructGraph(spectrum: List[Int]): List[(Int, Int, Int)] = {
    // Sort the spectrum to get vertices in ascending order
    val sortedSpectrum = spectrum.sorted
    
    // Create edges between consecutive vertices
    val edges = sortedSpectrum.zip(sortedSpectrum.tail).map { case (u, v) =>
      val weight = v - u
      (u, v, weight)
    }
    
    edges
  }
  
  def formatOutput(edges: List[(Int, Int, Int)]): String = {
    edges.map { case (u, v, w) => s"$u->$v:$w" }.mkString("\n")
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val spectrum = List(0, 113, 128, 241, 294, 407, 500)
    val graphEdges = constructGraph(spectrum)
    val output = formatOutput(graphEdges)
    
    println(output)
  }
}
```

## Explanation

The solution works as follows:

1. **Input Processing**: Takes a list of integers representing the spectrum
2. **Sorting**: Sorts the spectrum to ensure vertices are in ascending order
3. **Edge Creation**: Creates edges between consecutive vertices in the sorted spectrum
4. **Weight Calculation**: The weight of each edge is the difference between the two vertices
5. **Output Formatting**: Formats the result in the required format (u->v:weight)

## Example

For the spectrum `[0, 113, 128, 241, 294, 407, 500]`:

- Edge from 0 to 113 with weight 113
- Edge from 113 to 128 with weight 15
- Edge from 128 to 241 with weight 113
- Edge from 241 to 294 with weight 53
- Edge from 294 to 407 with weight 113
- Edge from 407 to 500 with weight 93

## Alternative Implementation

```scala
object ConstructTheGraphOfASpectrum {
  
  def constructGraph(spectrum: List[Int]): List[String] = {
    val sorted = spectrum.sorted
    
    sorted.zip(sorted.tail).map { case (u, v) =>
      s"$u->$v:${v - u}"
    }
  }
  
  def main(args: Array[String]): Unit = {
    val spectrum = List(0, 113, 128, 241, 294, 407, 500)
    val result = constructGraph(spectrum)
    
    result.foreach(println)
  }
}
```

This implementation is more concise and directly returns the formatted strings as required by the Rosalind problem format.

