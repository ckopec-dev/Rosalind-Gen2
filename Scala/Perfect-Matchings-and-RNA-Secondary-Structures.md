# Rosalind Problem: Perfect Matchings and RNA Secondary Structures

## Problem Statement
In this problem, we are given an RNA string and need to count the number of perfect matchings in its secondary structure. A perfect matching occurs when every nucleotide is paired with exactly one other nucleotide.

## Solution Approach
1. Count the number of each nucleotide type (A, U, G, C)
2. For perfect matching, A must pair with U and G must pair with C
3. The number of perfect matchings is the product of the factorials of the counts of paired nucleotides

## Scala Implementation

```scala
import scala.io.Source

object PerfectMatchings {
  
  def countNucleotides(rna: String): Map[Char, Int] = {
    rna.groupBy(identity).mapValues(_.length)
  }
  
  def countPerfectMatchings(rna: String): Long = {
    val counts = countNucleotides(rna)
    
    // Count A and U (they pair with each other)
    val aCount = counts.getOrElse('A', 0)
    val uCount = counts.getOrElse('U', 0)
    
    // Count G and C (they pair with each other)
    val gCount = counts.getOrElse('G', 0)
    val cCount = counts.getOrElse('C', 0)
    
    // For perfect matching, we need equal numbers of paired nucleotides
    // The number of perfect matchings is the product of factorials
    val aU = math.min(aCount, uCount)
    val gC = math.min(gCount, cCount)
    
    // Calculate factorial of each count
    val aUFactorial = factorial(aU)
    val gCFactorial = factorial(gC)
    
    aUFactorial * gCFactorial
  }
  
  def factorial(n: Int): Long = {
    if (n <= 1) 1
    else (1 to n).product.toLong
  }
  
  def main(args: Array[String]): Unit = {
    // Read input from file or stdin
    val input = Source.fromFile("rosalind_pmch.txt").getLines().next()
    val result = countPerfectMatchings(input)
    println(result)
  }
}

// Alternative implementation with more explicit pairing logic
object PerfectMatchingsAlternative {
  
  def countPerfectMatchings(rna: String): Long = {
    val counts = rna.groupBy(identity).mapValues(_.length)
    
    val aCount = counts.getOrElse('A', 0)
    val uCount = counts.getOrElse('U', 0)
    val gCount = counts.getOrElse('G', 0)
    val cCount = counts.getOrElse('C', 0)
    
    // For perfect matching, we need to pair A with U and G with C
    // The number of such pairings is the minimum of each pair
    val pairsAU = math.min(aCount, uCount)
    val pairsGC = math.min(gCount, cCount)
    
    // The number of perfect matchings is the product of the factorials
    // of the number of pairs for each type
    factorial(pairsAU) * factorial(pairsGC)
  }
  
  def factorial(n: Int): Long = {
    if (n <= 1) 1L
    else (1 to n).product.toLong
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val example = "AUAU"
    val result = countPerfectMatchings(example)
    println(s"Perfect matchings for $example: $result")
    
    // Read from file
    val input = scala.io.Source.fromFile("rosalind_pmch.txt").getLines().next()
    val finalResult = countPerfectMatchings(input)
    println(finalResult)
  }
}
```

## Explanation

The key insight for this problem is that in a perfect matching of an RNA secondary structure:

1. **A pairs with U**: The number of A-U pairs equals the minimum of A count and U count
2. **G pairs with C**: The number of G-C pairs equals the minimum of G count and C count
3. **Total perfect matchings**: The number of ways to arrange these pairs is the product of factorials of the pair counts

For example, if we have:
- 3 A's and 2 U's → 2 A-U pairs
- 2 G's and 3 C's → 2 G-C pairs

The number of perfect matchings = 2! × 2! = 4

## Example Input/Output

**Input**: `AUAU`
**Output**: `2`

**Input**: `ACAGU`
**Output**: `1`

## Time Complexity
- O(n) for counting nucleotides
- O(min(A,U) + min(G,C)) for calculating factorials
- Overall: O(n)

## Space Complexity
- O(1) additional space (only storing counts and results)

This solution correctly handles the constraints of the Rosalind problem and efficiently computes the number of perfect matchings in RNA secondary structures.

