# Rosalind Problem: Complementing a Strand of DNA (Scala Solution)

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. The reverse complement of a DNA string is formed by reversing the string and then taking the complement of each symbol.

Given: A DNA string s of length at most 1000 bp.

Return: The reverse complement s^c of s.

## Solution

```scala
object ComplementingDNA {
  def main(args: Array[String]): Unit = {
    // Example DNA string
    val dnaString = "AAAACCCGGT"
    
    // Get reverse complement
    val result = reverseComplement(dnaString)
    
    println(result)
  }
  
  def reverseComplement(dna: String): String = {
    dna.reverse.map(complement)
  }
  
  def complement(nucleotide: Char): Char = {
    nucleotide match {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
      case _ => throw new IllegalArgumentException(s"Invalid nucleotide: $nucleotide")
    }
  }
}
```

## Alternative Implementation Using Map

```scala
object ComplementingDNA {
  def main(args: Array[String]): Unit = {
    val dnaString = "AAAACCCGGT"
    val result = reverseComplement(dnaString)
    println(result)
  }
  
  def reverseComplement(dna: String): String = {
    val complementMap = Map(
      'A' -> 'T',
      'T' -> 'A',
      'C' -> 'G',
      'G' -> 'C'
    )
    
    dna.reverse.map(complementMap)
  }
}
```

## Step-by-step Explanation

1. **Input**: A DNA string consisting of nucleotides A, T, C, G
2. **Process**: 
   - Reverse the string
   - Replace each nucleotide with its complement:
     - A ↔ T
     - C ↔ G
3. **Output**: The reverse complement string

## Example Walkthrough

For input `"AAAACCCGGT"`:
1. Reverse the string: `"TGGCCCAAAT"`
2. Complement each nucleotide:
   - T → A
   - G → C
   - G → C
   - C → G
   - C → G
   - C → G
   - A → T
   - A → T
   - A → T
   - T → A
3. Result: `"ACCGGTTTAA"`

## Test Case

```scala
// Test the function
def testReverseComplement(): Unit = {
  assert(reverseComplement("AAAACCCGGT") == "ACCGGTTTAA")
  assert(reverseComplement("AAA") == "TTT")
  assert(reverseComplement("ACGT") == "ACGT")
  println("All tests passed!")
}
```

The solution uses Scala's functional programming features with `reverse` and `map` operations for clean, readable code.

