# Rosalind Problem: Implement_CycleToChromosome

## Problem Description
The cycle to chromosome problem involves converting a cycle in a genome graph into a chromosome. Given a cycle of even length, we need to extract the chromosome representation.

## Solution Approach
1. **Understand the cycle structure**: A cycle alternates between black edges and gray edges
2. **Extract chromosome**: For each pair of consecutive nodes in the cycle, we form a chromosome with signed integers
3. **Handle orientation**: The sign of each element depends on the relative positions in the cycle

## Scala Implementation

```scala
def cycleToChromosome(cycle: List[Int]): List[Int] = {
  val chromosome = new scala.collection.mutable.ListBuffer[Int]()
  
  // Process the cycle in pairs of consecutive nodes
  for (i <- 0 until cycle.length by 2) {
    val node1 = cycle(i)
    val node2 = cycle(i + 1)
    
    // Convert to chromosome representation
    // If node1 < node2, then the chromosome element is node2/2
    // If node1 > node2, then the chromosome element is -node1/2
    val element = if (node1 < node2) node2 / 2 else -node1 / 2
    chromosome += element
  }
  
  chromosome.toList
}

// Alternative implementation using zip and map
def cycleToChromosomeAlt(cycle: List[Int]): List[Int] = {
  cycle.grouped(2)
    .map { case List(a, b) => 
      if (a < b) b / 2 else -a / 2 
    }
    .toList
}

// Test function
def testCycleToChromosome(): Unit = {
  // Test case from Rosalind
  val cycle = List(1, 2, 4, 3, 6, 5, 7, 8)
  val result = cycleToChromosome(cycle)
  println(s"Input cycle: $cycle")
  println(s"Output chromosome: $result")
  // Expected: [1, -2, -3, 4]
  
  // Additional test
  val cycle2 = List(1, 2, 3, 4, 5, 6)
  val result2 = cycleToChromosome(cycle2)
  println(s"Input cycle: $cycle2")
  println(s"Output chromosome: $result2")
  // Expected: [1, 2, 3]
}
```

## Explanation

The algorithm works as follows:

1. **Pair Processing**: The cycle is processed in pairs of consecutive elements (since cycles alternate between black and gray edges)

2. **Chromosome Conversion**: For each pair (node1, node2):
   - If node1 < node2: the chromosome element is node2/2 (positive)
   - If node1 > node2: the chromosome element is -node1/2 (negative)

3. **Example Walkthrough**:
   - Input cycle: [1, 2, 4, 3, 6, 5, 7, 8]
   - Pair 1: (1, 2) → 1 < 2 → 2/2 = 1
   - Pair 2: (4, 3) → 4 > 3 → -4/2 = -2  
   - Pair 3: (6, 5) → 6 > 5 → -6/2 = -3
   - Pair 4: (7, 8) → 7 < 8 → 8/2 = 4
   - Result: [1, -2, -3, 4]

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the cycle
- **Space Complexity**: O(n) for storing the result chromosome

## Usage Example
```scala
// Example usage
val inputCycle = List(1, 2, 4, 3, 6, 5, 7, 8)
val chromosome = cycleToChromosome(inputCycle)
println(chromosome) // Output: List(1, -2, -3, 4)
```

This implementation correctly handles the cycle to chromosome conversion as required by the Rosalind problem.

