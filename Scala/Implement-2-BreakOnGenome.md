# Rosalind Problem: Implement 2-BreakOnGenome

## Problem Description
Implement the 2-BreakOnGenome algorithm to perform a 2-break operation on a genome.

## Solution

```scala
object BreakOnGenome {
  
  def twoBreakOnGenome(genome: List[List[Int]], i: Int, j: Int, k: Int, l: Int): List[List[Int]] = {
    // Convert genome to adjacency list representation
    val adjacencies = genome.flatMap(cycle => {
      if (cycle.length == 1) {
        List((cycle(0), -cycle(0)))
      } else {
        val extendedCycle = cycle :+ cycle(0)
        extendedCycle.zip(extendedCycle.tail).map { case (u, v) =>
          (u, v)
        }
      }
    })
    
    // Remove the edges (i,j) and (k,l)
    val filteredAdjacencies = adjacencies.filterNot { case (u, v) =>
      (u == i && v == j) || (u == j && v == i) ||
      (u == k && v == l) || (u == l && v == k)
    }
    
    // Add the new edges (i,k) and (j,l)
    val newAdjacencies = filteredAdjacencies :+ (i, k) :+ (j, l)
    
    // Convert back to cycles
    val cycles = convertToCycles(newAdjacencies)
    cycles
  }
  
  def convertToCycles(adjacencies: List[(Int, Int)]): List[List[Int]] = {
    val visited = scala.collection.mutable.Set[Int]()
    val cycles = scala.collection.mutable.ListBuffer[List[Int]]()
    
    val adjMap = adjacencies.groupBy(_._1).mapValues(_.map(_._2))
    
    for ((node, _) <- adjacencies) {
      if (!visited.contains(node)) {
        val cycle = scala.collection.mutable.ListBuffer[Int]()
        var current = node
        
        do {
          cycle += current
          visited += current
          val next = adjMap.getOrElse(current, List()).headOption
          current = next.getOrElse(0)
        } while (current != node && current != 0)
        
        if (cycle.length > 1) {
          cycles += cycle.toList
        }
      }
    }
    
    cycles.toList
  }
  
  // Alternative cleaner implementation
  def twoBreakOnGenomeClean(genome: List[List[Int]], i: Int, j: Int, k: Int, l: Int): List[List[Int]] = {
    // Convert genome to adjacency list
    val adjacencies = scala.collection.mutable.ListBuffer[(Int, Int)]()
    
    for (cycle <- genome) {
      if (cycle.length == 1) {
        adjacencies += ((cycle(0), -cycle(0)))
      } else {
        for (idx <- cycle.indices) {
          val u = cycle(idx)
          val v = cycle((idx + 1) % cycle.length)
          adjacencies += ((u, v))
        }
      }
    }
    
    // Remove old edges
    val filtered = adjacencies.filter { case (u, v) =>
      !((u == i && v == j) || (u == j && v == i) ||
        (u == k && v == l) || (u == l && v == k))
    }
    
    // Add new edges
    filtered += ((i, k))
    filtered += ((j, l))
    
    // Convert back to cycles
    convertToCycles(filtered.toList)
  }
  
  // Simple version for the specific problem
  def solve(i: Int, j: Int, k: Int, l: Int, genome: List[List[Int]]): List[List[Int]] = {
    twoBreakOnGenomeClean(genome, i, j, k, l)
  }
}

// Example usage:
// val genome = List(List(1, 2, 3, 4, 5, 6))
// val result = BreakOnGenome.solve(1, 6, 3, 4, genome)
```

## Explanation

The 2-BreakOnGenome algorithm performs a 2-break operation on a genome, which is a fundamental operation in genome rearrangement problems. 

**Algorithm Steps:**
1. Convert the genome (collection of cycles) into adjacency list representation
2. Remove the existing edges (i,j) and (k,l) 
3. Add new edges (i,k) and (j,l)
4. Convert the resulting adjacency list back into cycles

**Key Points:**
- The genome is represented as a list of cycles
- Each cycle is a list of integers (representing signed integers)
- A 2-break operation replaces two edges with two new edges
- The algorithm maintains the structure of the genome while performing the rearrangement

**Time Complexity:** O(n) where n is the total number of elements in the genome
**Space Complexity:** O(n) for storing the adjacency list representation

This implementation handles both circular and linear representations of genomic cycles and correctly performs the 2-break operation as required by the Rosalind problem.

