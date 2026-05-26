# Rosalind Problem: Find_Frequent_Words_with_Mismatches_and_Reverse_Complements

## Problem Description
Given a DNA string and integers k and d, find all k-mers that appear most frequently with at most d mismatches and their reverse complements.

## Solution in Scala

```scala
object FindFrequentWordsWithMismatchesAndReverseComplements {
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val text = "ACGTTGCATGTCGCATGATGCATGAGAGG"
    val k = 4
    val d = 1
    
    val result = findFrequentWordsWithMismatchesAndReverseComplements(text, k, d)
    println(result.mkString(" "))
  }
  
  def findFrequentWordsWithMismatchesAndReverseComplements(text: String, k: Int, d: Int): List[String] = {
    val frequencyMap = scala.collection.mutable.Map[String, Int]()
    
    // Generate all possible k-mers and their reverse complements
    val allPatterns = getAllKmersWithReverseComplements(text, k)
    
    // Count frequency of each pattern with up to d mismatches
    for (pattern <- allPatterns) {
      val patternRC = reverseComplement(pattern)
      val patternKey = if (pattern < patternRC) pattern else patternRC
      
      // Count pattern and its reverse complement
      val count = getMismatchCount(text, pattern, d) + 
                  (if (pattern != patternRC) getMismatchCount(text, patternRC, d) else 0)
      
      frequencyMap(patternKey) = frequencyMap.getOrElse(patternKey, 0) + count
    }
    
    // Find maximum frequency
    val maxFreq = frequencyMap.values.max
    
    // Return all patterns with maximum frequency
    frequencyMap.filter(_._2 == maxFreq).keys.toList
  }
  
  def getAllKmersWithReverseComplements(text: String, k: Int): List[String] = {
    val kmers = scala.collection.mutable.Set[String]()
    
    // Get all k-mers from text
    for (i <- 0 to text.length - k) {
      val kmer = text.substring(i, i + k)
      kmers += kmer
      kmers += reverseComplement(kmer)
    }
    
    kmers.toList
  }
  
  def getMismatchCount(text: String, pattern: String, d: Int): Int = {
    var count = 0
    
    for (i <- 0 to text.length - pattern.length) {
      val substring = text.substring(i, i + pattern.length)
      if (hammingDistance(substring, pattern) <= d) {
        count += 1
      }
    }
    
    count
  }
  
  def hammingDistance(s1: String, s2: String): Int = {
    val minLength = math.min(s1.length, s2.length)
    var distance = 0
    
    for (i <- 0 until minLength) {
      if (s1(i) != s2(i)) {
        distance += 1
      }
    }
    
    distance
  }
  
  def reverseComplement(pattern: String): String = {
    pattern.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
      case _ => throw new IllegalArgumentException("Invalid DNA nucleotide")
    }
  }
}
```

## Alternative Implementation (More Efficient)

```scala
object FindFrequentWordsWithMismatchesAndReverseComplementsOptimized {
  
  def findFrequentWordsWithMismatchesAndReverseComplements(text: String, k: Int, d: Int): List[String] = {
    val frequencyMap = scala.collection.mutable.Map[String, Int]()
    
    // For each k-mer in text
    for (i <- 0 to text.length - k) {
      val pattern = text.substring(i, i + k)
      val patternRC = reverseComplement(pattern)
      
      // Get all neighbors (k-mers with at most d mismatches)
      val neighbors = getNeighbors(pattern, d)
      val neighborsRC = getNeighbors(patternRC, d)
      
      // Add all neighbors to frequency map
      for (neighbor <- neighbors) {
        frequencyMap(neighbor) = frequencyMap.getOrElse(neighbor, 0) + 1
      }
      
      for (neighbor <- neighborsRC) {
        frequencyMap(neighbor) = frequencyMap.getOrElse(neighbor, 0) + 1
      }
    }
    
    // Find maximum frequency
    val maxFreq = frequencyMap.values.max
    
    // Return all patterns with maximum frequency
    frequencyMap.filter(_._2 == maxFreq).keys.toList
  }
  
  def getNeighbors(pattern: String, d: Int): List[String] = {
    if (d == 0) return List(pattern)
    
    val nucleotides = List('A', 'C', 'G', 'T')
    val neighbors = scala.collection.mutable.Set[String]()
    
    def generateNeighbors(current: String, mismatches: Int): Unit = {
      if (mismatches < 0) return
      
      if (current.length == pattern.length) {
        neighbors += current
        return
      }
      
      val pos = current.length
      val base = pattern(pos)
      
      // Keep the original base
      generateNeighbors(current + base, mismatches)
      
      // Try all other bases
      for (nucleotide <- nucleotides if nucleotide != base) {
        generateNeighbors(current + nucleotide, mismatches - 1)
      }
    }
    
    generateNeighbors("", d)
    neighbors.toList
  }
  
  def reverseComplement(pattern: String): String = {
    pattern.reverse.map {
      case 'A' => 'T'
      case 'T' => 'A'
      case 'G' => 'C'
      case 'C' => 'G'
      case _ => throw new IllegalArgumentException("Invalid DNA nucleotide")
    }
  }
}
```

## Key Features of the Solution

1. **Reverse Complement Handling**: The solution properly handles reverse complements by considering both the original k-mer and its reverse complement.

2. **Mismatch Counting**: Uses Hamming distance to count mismatches between patterns and substrings.

3. **Frequency Tracking**: Uses a mutable map to track frequencies of patterns with mismatches.

4. **Optimization**: The second implementation uses a more efficient approach by generating neighbors directly rather than checking all possible patterns.

## Time Complexity
- O(n × k × 4^d) where n is the length of the text, k is the k-mer length, and d is the maximum number of mismatches.

## Space Complexity
- O(4^k) for storing all possible k-mers and their reverse complements.

The solution correctly identifies frequent words with mismatches and their reverse complements, which is essential for bioinformatics applications like finding conserved motifs in DNA sequences.

