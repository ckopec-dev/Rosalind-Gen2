# Rosalind Problem: Identifying Reversing Substitutions

## Problem Description
Given two DNA strings of equal length, find the positions where the strings differ and determine if the substitution is a reversing substitution (purine ↔ pyrimidine).

## Solution in Scala

```scala
object IdentifyingReversingSubstitutions {
  
  def main(args: Array[String]): Unit = {
    // Example DNA strings
    val dna1 = "ACGTACGTACGT"
    val dna2 = "ACGTTCGTACGT"
    
    val result = findReversingSubstitutions(dna1, dna2)
    println(s"Reversing substitutions at positions: ${result.mkString(", ")}")
  }
  
  /**
   * Find positions where DNA strings differ and are reversing substitutions
   * @param dna1 First DNA string
   * @param dna2 Second DNA string
   * @return List of positions (0-indexed) where reversing substitutions occur
   */
  def findReversingSubstitutions(dna1: String, dna2: String): List[Int] = {
    // Check if strings have equal length
    if (dna1.length != dna2.length) {
      throw new IllegalArgumentException("DNA strings must have equal length")
    }
    
    // Define purine and pyrimidine nucleotides
    val purines = Set('A', 'G')
    val pyrimidines = Set('C', 'T')
    
    // Helper function to check if two nucleotides form a reversing substitution
    def isReversingSubstitution(nuc1: Char, nuc2: Char): Boolean = {
      val nuc1Upper = nuc1.toUpper
      val nuc2Upper = nuc2.toUpper
      
      // Check if they are different nucleotides
      if (nuc1Upper == nuc2Upper) false
      else {
        // Check if one is purine and other is pyrimidine
        (purines.contains(nuc1Upper) && pyrimidines.contains(nuc2Upper)) ||
        (pyrimidines.contains(nuc1Upper) && purines.contains(nuc2Upper))
      }
    }
    
    // Find all positions where substitutions occur
    (0 until dna1.length)
      .filter(i => isReversingSubstitution(dna1(i), dna2(i)))
      .toList
  }
  
  /**
   * Alternative implementation using zip and map
   */
  def findReversingSubstitutionsAlt(dna1: String, dna2: String): List[Int] = {
    if (dna1.length != dna2.length) {
      throw new IllegalArgumentException("DNA strings must have equal length")
    }
    
    val purines = Set('A', 'G')
    val pyrimidines = Set('C', 'T')
    
    def isReversing(nuc1: Char, nuc2: Char): Boolean = {
      val n1 = nuc1.toUpper
      val n2 = nuc2.toUpper
      n1 != n2 && 
      ((purines.contains(n1) && pyrimidines.contains(n2)) || 
       (pyrimidines.contains(n1) && purines.contains(n2)))
    }
    
    dna1.zip(dna2)
      .zipWithIndex
      .filter { case ((nuc1, nuc2), _) => isReversing(nuc1, nuc2) }
      .map(_._2)
      .toList
  }
}
```

## Example Usage

```scala
// Test with sample data
val seq1 = "ACGTACGTACGT"
val seq2 = "ACGTTCGTACGT"

val positions = findReversingSubstitutions(seq1, seq2)
println(positions) // Output: List(4)

// Another example
val seq3 = "AAAA"
val seq4 = "TTTT"

val positions2 = findReversingSubstitutions(seq3, seq4)
println(positions2) // Output: List(0, 1, 2, 3)
```

## Explanation

1. **Problem Understanding**: We need to identify positions where two DNA sequences differ, and specifically find positions where the substitution is a "reversing substitution" - meaning one nucleotide is a purine (A or G) and the other is a pyrimidine (C or T).

2. **Key Concepts**:
   - Purines: A, G
   - Pyrimidines: C, T
   - Reversing substitution: purine ↔ pyrimidine

3. **Algorithm Steps**:
   - Compare each position in both DNA strings
   - Check if nucleotides differ
   - Verify if one is purine and other is pyrimidine
   - Return list of positions where this occurs

4. **Time Complexity**: O(n) where n is the length of the DNA strings
5. **Space Complexity**: O(k) where k is the number of reversing substitutions found

## Sample Input/Output

**Input**:
```
ACGTACGTACGT
ACGTTCGTACGT
```

**Output**:
```
List(4)
```

**Explanation**: At position 4, we have 'A' vs 'T'. Since 'A' is a purine and 'T' is a pyrimidine, this is a reversing substitution.

