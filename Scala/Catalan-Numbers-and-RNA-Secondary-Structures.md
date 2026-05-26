# Rosalind Problem: Catalan Numbers and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the number of possible secondary structures for an RNA sequence, which is equivalent to finding the nth Catalan number where n is the number of base pairs in the structure.

## Approach

The key insight is that valid RNA secondary structures follow the rules of base pairing (A-U, U-A, G-C, C-G) and cannot have pseudoknots. The number of such structures is given by the Catalan numbers.

For a sequence of length n, if we consider the number of ways to form base pairs, this becomes a classic Catalan number problem.

## Solution

```scala
object CatalanNumbersAndRNASecondaryStructures {
  
  def catalanNumber(n: Int): Long = {
    if (n <= 1) return 1
    
    // Using the formula: C(n) = (2n)! / ((n+1)! * n!)
    // But we'll use a more efficient approach to avoid large factorials
    
    val dp = new Array[Long](n + 1)
    dp(0) = 1
    dp(1) = 1
    
    for (i <- 2 to n) {
      dp(i) = 0
      for (j <- 0 until i) {
        dp(i) += dp(j) * dp(i - 1 - j)
      }
    }
    
    dp(n)
  }
  
  def countValidSecondaryStructures(sequence: String): Long = {
    // For RNA secondary structures, we need to count valid base pairings
    // This is equivalent to finding the nth Catalan number where n is the number of base pairs
    
    // Count the number of valid base pairs (A-U, U-A, G-C, C-G)
    val n = sequence.length / 2  // Assuming we're looking for the number of base pairs
    
    // For the general case, we need to compute the number of valid secondary structures
    // This is the nth Catalan number
    
    catalanNumber(n)
  }
  
  // More precise approach for the specific problem
  def countRNASecondaryStructures(sequence: String): Long = {
    // The number of valid RNA secondary structures is the nth Catalan number
    // where n is the number of base pairs
    
    // For a sequence of length n, if we consider the maximum number of base pairs
    // that can be formed, it's n/2 (assuming perfect pairing)
    
    val n = sequence.length / 2
    
    if (n <= 1) return 1
    
    // Using dynamic programming to compute Catalan numbers
    val catalan = new Array[Long](n + 1)
    catalan(0) = 1
    catalan(1) = 1
    
    for (i <- 2 to n) {
      for (j <- 0 until i) {
        catalan(i) += catalan(j) * catalan(i - 1 - j)
      }
    }
    
    catalan(n)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val sequence = "AUCG"
    val result = countRNASecondaryStructures(sequence)
    println(s"Number of secondary structures for $sequence: $result")
    
    // Test with different sequences
    val testSequences = List("AUCG", "AUCGAUCG", "UUUU")
    
    testSequences.foreach { seq =>
      val count = countRNASecondaryStructures(seq)
      println(s"Sequence: $seq, Secondary structures: $count")
    }
  }
}

// Alternative implementation using memoization
object CatalanMemoized {
  import scala.collection.mutable
  
  def catalanMemoized(n: Int, memo: mutable.Map[Int, Long] = mutable.Map()): Long = {
    if (n <= 1) return 1
    
    if (memo.contains(n)) return memo(n)
    
    var result = 0L
    for (i <- 0 until n) {
      result += catalanMemoized(i, memo) * catalanMemoized(n - 1 - i, memo)
    }
    
    memo(n) = result
    result
  }
  
  def countSecondaryStructures(sequence: String): Long = {
    val n = sequence.length / 2
    catalanMemoized(n)
  }
}
```

## Explanation

1. **Catalan Numbers**: The number of valid RNA secondary structures follows the Catalan number sequence. For n base pairs, the number of valid structures is the nth Catalan number.

2. **Dynamic Programming Approach**: We use dynamic programming to compute Catalan numbers efficiently:
   - C(0) = 1, C(1) = 1
   - C(n) = Σ(i=0 to n-1) C(i) × C(n-1-i)

3. **RNA Structure Counting**: For an RNA sequence, we count the number of valid base pairings, which corresponds to the number of ways to arrange parentheses (valid secondary structures).

## Time and Space Complexity

- **Time Complexity**: O(n²) for computing the nth Catalan number using dynamic programming
- **Space Complexity**: O(n) for the DP array

## Sample Input/Output

For an RNA sequence like "AUCG":
- Length = 4
- Number of base pairs = 2
- Second Catalan number = 2

The solution handles the mathematical relationship between RNA secondary structures and Catalan numbers efficiently.

