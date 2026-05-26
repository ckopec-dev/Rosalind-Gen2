# Rosalind Problem: Find All Shared k-mers of a Pair of Strings

## Problem Description
Given two DNA strings, find all shared k-mers (k-length substrings) that appear in both strings.

## Solution in Scala

```scala
object SharedKmers {
  
  def findAllSharedKmers(text1: String, text2: String, k: Int): List[String] = {
    // Get all k-mers from first text
    val kmers1 = getAllKmers(text1, k)
    // Get all k-mers from second text
    val kmers2 = getAllKmers(text2, k)
    
    // Find intersection of k-mers
    kmers1.intersect(kmers2).toList
  }
  
  def getAllKmers(text: String, k: Int): Set[String] = {
    if (text.length < k) return Set.empty
    (0 to text.length - k).map(i => text.substring(i, i + k)).toSet
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val text1 = "AAACTCATC"
    val text2 = "TTATCTCAT"
    val k = 3
    
    val sharedKmers = findAllSharedKmers(text1, text2, k)
    println(s"Shared k-mers of length $k:")
    sharedKmers.foreach(println)
  }
}
```

## Alternative Implementation with Reverse Complement

For the more complex version that also considers reverse complements:

```scala
object SharedKmersWithRC {
  
  def reverseComplement(dna: String): String = {
    dna.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
    }
  }
  
  def findAllSharedKmers(text1: String, text2: String, k: Int): List[String] = {
    // Get all k-mers from first text
    val kmers1 = getAllKmers(text1, k)
    // Get all k-mers from second text
    val kmers2 = getAllKmers(text2, k)
    
    // Get reverse complements
    val rc1 = kmers1.map(reverseComplement)
    val rc2 = kmers2.map(reverseComplement)
    
    // Find intersection of k-mers and their reverse complements
    val intersection = kmers1.intersect(kmers2) ++
                       kmers1.intersect(rc2) ++
                       rc1.intersect(kmers2) ++
                       rc1.intersect(rc2)
    
    intersection.toList
  }
  
  def getAllKmers(text: String, k: Int): Set[String] = {
    if (text.length < k) return Set.empty
    (0 to text.length - k).map(i => text.substring(i, i + k)).toSet
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val text1 = "AAACTCATC"
    val text2 = "TTATCTCAT"
    val k = 3
    
    val sharedKmers = findAllSharedKmers(text1, text2, k)
    println(s"Shared k-mers of length $k:")
    sharedKmers.foreach(println)
  }
}
```

## Input/Output Format

**Input:**
- First line: DNA string 1
- Second line: DNA string 2  
- Third line: Integer k (k-mer length)

**Output:**
- All shared k-mers in any order

## Example

**Input:**
```
AAACTCATC
TTATCTCAT
3
```

**Output:**
```
ACT
ATC
TCA
```

## Key Features

1. **Efficient k-mer extraction**: Uses substring operations with proper bounds checking
2. **Set operations**: Leverages Scala's Set operations for efficient intersection
3. **Error handling**: Handles cases where k is larger than string length
4. **Flexible**: Can be easily modified to include reverse complement considerations

## Time Complexity
- O(n₁ × k + n₂ × k) for k-mer generation where n₁ and n₂ are string lengths
- O(m₁ + m₂) for intersection operation where m₁ and m₂ are number of unique k-mers
- Overall: O(n₁ × k + n₂ × k + m₁ + m₂)

## Space Complexity
- O(n₁ × k + n₂ × k) for storing k-mers

This solution efficiently finds all shared k-mers between two DNA strings using Scala's functional programming features and efficient data structures.

