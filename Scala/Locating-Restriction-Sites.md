# Rosalind Problem: Locating Restriction Sites

## Problem Statement
In DNA strings, symbols 'A' and 'T' are complements of each other, as are 'C' and 'G'. The reverse complement of a DNA string s is the string sc formed by reversing the symbols of s, then taking the complement of each symbol (e.g., the reverse complement of "GTCA" is "TGAC").

Given a DNA string of length at most 1 kbp, return all locations of restriction sites of length 4-12. A restriction site is a location where a DNA string is equal to its reverse complement.

## Solution in Scala

```scala
object LocatingRestrictionSites {
  
  def reverseComplement(dna: String): String = {
    dna.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
      case _ => throw new IllegalArgumentException("Invalid DNA nucleotide")
    }
  }
  
  def isRestrictionSite(dna: String): Boolean = {
    dna == reverseComplement(dna)
  }
  
  def findRestrictionSites(dna: String): List[(Int, Int)] = {
    val sites = scala.collection.mutable.ListBuffer[(Int, Int)]()
    
    // Check all possible substring lengths from 4 to 12
    for (length <- 4 to 12) {
      // Check all possible starting positions for this length
      for (i <- 0 to dna.length - length) {
        val substring = dna.substring(i, i + length)
        if (isRestrictionSite(substring)) {
          sites += ((i + 1, length)) // 1-indexed positions
        }
      }
    }
    
    sites.toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val dna = "TCAATGCATGCGGGTCTATATGCAT"
    val sites = findRestrictionSites(dna)
    
    sites.foreach { case (position, length) =>
      println(s"$position $length")
    }
  }
}
```

## Alternative Implementation (More Functional Style)

```scala
object LocatingRestrictionSitesFunctional {
  
  def reverseComplement(dna: String): String = {
    dna.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'C' => 'G'
      case 'G' => 'C'
      case _ => throw new IllegalArgumentException("Invalid DNA nucleotide")
    }
  }
  
  def isRestrictionSite(dna: String): Boolean = {
    dna == reverseComplement(dna)
  }
  
  def findRestrictionSites(dna: String): List[(Int, Int)] = {
    (4 to 12)
      .flatMap(length => {
        (0 to dna.length - length)
          .collect {
            case i if isRestrictionSite(dna.substring(i, i + length)) =>
              (i + 1, length) // 1-indexed positions
          }
      })
      .toList
  }
  
  def main(args: Array[String]): Unit = {
    val dna = "TCAATGCATGCGGGTCTATATGCAT"
    val sites = findRestrictionSites(dna)
    
    sites.foreach { case (position, length) =>
      println(s"$position $length")
    }
  }
}
```

## Explanation

1. **reverseComplement function**: Takes a DNA string and returns its reverse complement by:
   - Reversing the string
   - Mapping each nucleotide to its complement (A↔T, C↔G)

2. **isRestrictionSite function**: Checks if a DNA string is equal to its reverse complement

3. **findRestrictionSites function**: 
   - Iterates through all possible substring lengths from 4 to 12
   - For each length, checks all possible starting positions
   - Returns a list of tuples containing (position, length) for all restriction sites
   - Positions are 1-indexed as required by the problem

## Time Complexity
- O(n²) where n is the length of the DNA string
- For each length from 4 to 12, we check O(n) substrings
- Each substring check takes O(length) time

## Space Complexity
- O(1) additional space (excluding output storage)

The solution correctly identifies all restriction sites where a DNA sequence equals its reverse complement, which are palindromic sequences in DNA.

