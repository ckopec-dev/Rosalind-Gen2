# Rosalind Problem: Find the Most Frequent Words with Mismatches in a String

## Problem Description
Given a string Text and integers k and d, we need to find all k-mers that appear in Text with at most d mismatches.

## Solution in Scala

```scala
object FrequentWordsWithMismatches {
  
  def frequencyMap(text: String, k: Int, d: Int): Map[String, Int] = {
    val frequency = scala.collection.mutable.Map[String, Int]()
    
    // Generate all possible k-mers
    val kmers = generateAllKmers(k)
    
    // For each k-mer, count occurrences with up to d mismatches
    for (kmer <- kmers) {
      val count = approximatePatternCount(text, kmer, d)
      if (count > 0) {
        frequency(kmer) = count
      }
    }
    
    frequency.toMap
  }
  
  def approximatePatternCount(text: String, pattern: String, d: Int): Int = {
    var count = 0
    val n = text.length
    val k = pattern.length
    
    for (i <- 0 to n - k) {
      val substring = text.substring(i, i + k)
      if (hammingDistance(substring, pattern) <= d) {
        count += 1
      }
    }
    
    count
  }
  
  def hammingDistance(pattern1: String, pattern2: String): Int = {
    val minLen = math.min(pattern1.length, pattern2.length)
    var distance = 0
    
    for (i <- 0 until minLen) {
      if (pattern1(i) != pattern2(i)) {
        distance += 1
      }
    }
    
    distance
  }
  
  def generateAllKmers(k: Int): List[String] = {
    if (k == 0) return List("")
    
    val nucleotides = List('A', 'C', 'G', 'T')
    val kmers = scala.collection.mutable.ListBuffer[String]()
    
    def generate(current: String, remaining: Int): Unit = {
      if (remaining == 0) {
        kmers += current
        return
      }
      
      for (nucleotide <- nucleotides) {
        generate(current + nucleotide, remaining - 1)
      }
    }
    
    generate("", k)
    kmers.toList
  }
  
  def mostFrequentWordsWithMismatches(text: String, k: Int, d: Int): List[String] = {
    val frequency = frequencyMap(text, k, d)
    val maxCount = frequency.values.max
    frequency.filter(_._2 == maxCount).keys.toList
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
    val k = 4
    val d = 1
    
    val result = mostFrequentWordsWithMismatches(text, k, d)
    println(s"Most frequent words with mismatches: ${result.mkString(" ")}")
  }
}
```

## Alternative Optimized Solution

```scala
object FrequentWordsWithMismatchesOptimized {
  
  def mostFrequentWordsWithMismatches(text: String, k: Int, d: Int): List[String] = {
    val frequency = scala.collection.mutable.Map[String, Int]()
    
    // For each k-mer in the text
    for (i <- 0 to text.length - k) {
      val pattern = text.substring(i, i + k)
      
      // Generate all k-mers with at most d mismatches
      val neighbors = neighbors(pattern, d)
      
      for (neighbor <- neighbors) {
        frequency(neighbor) = frequency.getOrElse(neighbor, 0) + 1
      }
    }
    
    val maxCount = frequency.values.max
    frequency.filter(_._2 == maxCount).keys.toList
  }
  
  def neighbors(pattern: String, d: Int): Set[String] = {
    if (d == 0) return Set(pattern)
    
    val nucleotides = List('A', 'C', 'G', 'T')
    val neighbors = scala.collection.mutable.Set[String]()
    
    def getNeighbors(current: String, mismatches: Int): Unit = {
      if (mismatches == 0) {
        neighbors += current
        return
      }
      
      for (i <- current.indices) {
        for (nucleotide <- nucleotides) {
          if (current(i) != nucleotide) {
            val newPattern = current.updated(i, nucleotide)
            getNeighbors(newPattern, mismatches - 1)
          }
        }
      }
    }
    
    getNeighbors(pattern, d)
    neighbors.toSet
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val text = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
    val k = 4
    val d = 1
    
    val result = mostFrequentWordsWithMismatches(text, k, d)
    println(s"Most frequent words with mismatches: ${result.mkString(" ")}")
  }
}
```

## Explanation

### Key Functions:

1. **`approximatePatternCount`**: Counts how many times a pattern appears in text with at most d mismatches
2. **`hammingDistance`**: Calculates the number of differences between two strings
3. **`generateAllKmers`**: Generates all possible k-mers of length k
4. **`frequencyMap`**: Creates a map of all k-mers and their frequencies with mismatches
5. **`mostFrequentWordsWithMismatches`**: Finds the most frequent k-mers with at most d mismatches

### Approach:

1. For each k-mer in the text, find all k-mers that are within d mismatches
2. Count the frequency of each k-mer with mismatches
3. Return the k-mers with maximum frequency

### Time Complexity:
- O(n × k × 4^k) where n is the length of text and k is the k-mer length
- This is due to generating all possible k-mers and checking each with mismatches

### Space Complexity:
- O(4^k) for storing all possible k-mers

The optimized version uses a neighbor generation approach which can be more efficient for larger datasets.

