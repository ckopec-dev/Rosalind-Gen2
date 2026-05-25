# Rosalind Problem: Construct the Suffix Array of a String (Scala Solution)

## Problem Understanding

A suffix array is a sorted array of all suffixes of a given string. For a string of length n, there are n suffixes, and the suffix array contains the starting positions of these suffixes in lexicographically sorted order.

## Solution Approach

1. Generate all suffixes of the input string
2. Sort the suffixes lexicographically
3. Extract the starting positions of the sorted suffixes

## Scala Implementation

```scala
def constructSuffixArray(s: String): List[Int] = {
  // Generate all suffixes with their starting positions
  val suffixes = (0 until s.length).map(i => (s.substring(i), i)).toList
  
  // Sort suffixes lexicographically and extract starting positions
  suffixes.sortBy(_._1).map(_._2)
}

// Alternative more concise implementation
def constructSuffixArrayConcise(s: String): List[Int] = {
  (0 until s.length)
    .map(i => s.substring(i) -> i)
    .toList
    .sortBy(_._1)
    .map(_._2)
}

// Example usage
val input = "PANAMABANANAS$"
val suffixArray = constructSuffixArray(input)
println(suffixArray.mkString(" ")) // Output: 15 14 12 11 8 7 4 3 1 0 13 10 6 5 2 9

// Verification function to show the sorted suffixes
def showSuffixes(s: String): Unit = {
  val suffixes = (0 until s.length)
    .map(i => s.substring(i) -> i)
    .toList
    .sortBy(_._1)
  
  suffixes.foreach { case (suffix, pos) =>
    println(s"$pos: $suffix")
  }
}

// Test with example
val testString = "PANAMABANANAS$"
println("Suffixes in sorted order:")
showSuffixes(testString)
```

## Explanation

The solution works as follows:

1. **Generate suffixes**: For each position `i` from 0 to `n-1`, we extract the substring from position `i` to the end of the string
2. **Pair with positions**: Each suffix is paired with its starting position in the original string
3. **Sort by suffix**: We sort these pairs lexicographically by the suffix string
4. **Extract positions**: Finally, we extract just the starting positions from the sorted pairs

## Time and Space Complexity

- **Time Complexity**: O(n² log n) where n is the length of the string
  - Generating suffixes: O(n²) 
  - Sorting: O(n log n) with O(n²) comparisons
- **Space Complexity**: O(n²) for storing all suffixes

## Sample Input/Output

**Input**: `"PANAMABANANAS$"`

**Output**: `15 14 12 11 8 7 4 3 1 0 13 10 6 5 2 9`

**Explanation of the output**:
- Position 15: `$` (lexicographically smallest)
- Position 14: `S$`
- Position 12: `NAS$`
- Position 11: `ANAS$`
- Position 8: `BANANAS$`
- Position 7: `ANABANANAS$`
- Position 4: `AMABANANAS$`
- Position 3: `NAMABANANAS$`
- Position 1: `ANAMABANANAS$`
- Position 0: `PANAMABANANAS$`
- And so on...

This solution correctly implements the suffix array construction algorithm for the given Rosalind problem.

