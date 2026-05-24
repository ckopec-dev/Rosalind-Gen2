# Rosalind Problem: Counting Point Mutations

## Problem Statement
Given two DNA strings of equal length, count the number of positions where the nucleotides differ (Hamming distance).

## Solution in Scala

```scala
def countPointMutations(dna1: String, dna2: String): Int = {
  dna1.zip(dna2).count { case (nucleotide1, nucleotide2) =>
    nucleotide1 != nucleotide2
  }
}

// Alternative implementation using for comprehension
def countPointMutationsAlt(dna1: String, dna2: String): Int = {
  (for {
    (n1, n2) <- dna1.zip(dna2)
  } yield if (n1 != n2) 1 else 0).sum
}

// Example usage
val sequence1 = "GAGCCTACTAACGGGAT"
val sequence2 = "CATCGGTATCGGGTTAT"
val mutations = countPointMutations(sequence1, sequence2)
println(s"Number of point mutations: $mutations") // Output: 7
```

## Explanation

The solution works by:

1. **Zipping the strings**: `dna1.zip(dna2)` creates pairs of corresponding nucleotides from both strings
2. **Counting differences**: `.count { case (nucleotide1, nucleotide2) => nucleotide1 != nucleotide2 }` counts how many pairs have different nucleotides
3. **Pattern matching**: The `case (nucleotide1, nucleotide2)` pattern matches each pair and compares them

## Alternative Approaches

```scala
// Using indices
def countPointMutationsWithIndices(dna1: String, dna2: String): Int = {
  dna1.indices.count(i => dna1(i) != dna2(i))
}

// Using fold
def countPointMutationsWithFold(dna1: String, dna2: String): Int = {
  dna1.zip(dna2).foldLeft(0) { case (count, (n1, n2)) =>
    if (n1 != n2) count + 1 else count
  }
}
```

## Test Cases

```scala
// Test the function
assert(countPointMutations("GAGCCTACTAACGGGAT", "CATCGGTATCGGGTTAT") == 7)
assert(countPointMutations("AAAA", "TTTT") == 4)
assert(countPointMutations("ACGT", "ACGT") == 0)
assert(countPointMutations("A", "T") == 1)
```

## Time and Space Complexity
- **Time Complexity**: O(n) where n is the length of the DNA strings
- **Space Complexity**: O(n) for creating the zipped pairs

The solution is efficient and handles the typical constraints of Rosalind problems where DNA sequences are usually in the range of hundreds to thousands of nucleotides.

