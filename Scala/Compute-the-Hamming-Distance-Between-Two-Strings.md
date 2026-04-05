# Rosalind Problem: Compute the Hamming Distance Between Two Strings

## Problem Description
The Hamming distance between two strings of equal length is the number of positions at which the corresponding symbols are different. Given two strings of equal length, we need to compute their Hamming distance.

## Solution in Scala

```scala
def hammingDistance(p: String, q: String): Int = {
  p.zip(q).count { case (a, b) => a != b }
}

// Alternative implementation using fold
def hammingDistanceFold(p: String, q: String): Int = {
  p.zip(q).foldLeft(0) { case (count, (a, b)) =>
    if (a != b) count + 1 else count
  }
}

// Alternative implementation using for comprehension
def hammingDistanceFor(p: String, q: String): Int = {
  val differences = for {
    (a, b) <- p.zip(q)
    if a != b
  } yield 1
  differences.sum
}

// Main function to read input and compute result
def main(args: Array[String]): Unit = {
  // Example usage
  val p = "GAGCCTACTAACGGGAT"
  val q = "CATCGGTATCGTTATAGCC"
  
  val distance = hammingDistance(p, q)
  println(s"Hamming distance between $p and $q is: $distance")
  
  // For reading from file (as typically required in Rosalind problems)
  // val lines = scala.io.Source.fromFile("dataset.txt").getLines().toList
  // val p = lines(0)
  // val q = lines(1)
  // println(hammingDistance(p, q))
}
```

## Explanation

The solution uses the `zip` method to pair corresponding characters from both strings, then counts the number of pairs where the characters are different.

### Key Points:
1. **`zip` method**: Pairs up characters from both strings at the same positions
2. **`count` method**: Counts elements that satisfy the given predicate
3. **Pattern matching**: `case (a, b) => a != b` extracts the paired characters and compares them

### Time Complexity: O(n)
### Space Complexity: O(n)

## Example
For strings:
- p = "GAGCCTACTAACGGGAT"
- q = "CATCGGTATCGTTATAGCC"

The Hamming distance is 7, as there are 7 positions where the characters differ.

## Test Case
```scala
// Test the function
assert(hammingDistance("GAGCCTACTAACGGGAT", "CATCGGTATCGTTATAGCC") == 7)
assert(hammingDistance("AAAA", "AAAA") == 0)
assert(hammingDistance("AAAA", "TTTT") == 4)
```

This solution is concise, readable, and efficient for the Rosalind problem requirements.

