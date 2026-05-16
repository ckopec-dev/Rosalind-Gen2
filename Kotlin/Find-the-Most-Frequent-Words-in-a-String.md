# Rosalind Problem: Find the Most Frequent Words in a String

## Problem Description
Find the most frequent k-mers (substrings of length k) in a given DNA string.

## Solution

```kotlin
fun findMostFrequentWords(dna: String, k: Int): List<String> {
    // Handle edge cases
    if (dna.isEmpty() || k <= 0 || k > dna.length) {
        return emptyList()
    }
    
    // Count frequency of each k-mer
    val kmerCount = mutableMapOf<String, Int>()
    
    // Iterate through all possible k-mers
    for (i in 0..dna.length - k) {
        val kmer = dna.substring(i, i + k)
        kmerCount[kmer] = kmerCount.getOrDefault(kmer, 0) + 1
    }
    
    // Find maximum frequency
    val maxCount = kmerCount.values.maxOrNull() ?: 0
    
    // Return all k-mers with maximum frequency
    return kmerCount.filter { it.value == maxCount }.keys.toList()
}

// Alternative implementation using groupBy
fun findMostFrequentWordsAlternative(dna: String, k: Int): List<String> {
    if (dna.isEmpty() || k <= 0 || k > dna.length) {
        return emptyList()
    }
    
    // Generate all k-mers and count them
    val kmerList = (0..dna.length - k)
        .map { dna.substring(it, it + k) }
    
    // Group by k-mer and count occurrences
    val kmerCount = kmerList.groupBy { it }.mapValues { it.value.size }
    
    // Find maximum count and return all k-mers with that count
    val maxCount = kmerCount.values.maxOrNull() ?: 0
    return kmerCount.filter { it.value == maxCount }.keys.toList()
}

// Example usage
fun main() {
    val dna = "ACGTTGCATGTCGCATGATGCATGAGAGCT"
    val k = 4
    
    val result = findMostFrequentWords(dna, k)
    println("Most frequent k-mers: ${result.joinToString(" ")}")
    
    // Test with the example from Rosalind
    val testDna = "CGATATATCCATAG"
    val testK = 3
    
    val testResult = findMostFrequentWords(testDna, testK)
    println("Test result: ${testResult.joinToString(" ")}")
}
```

## Explanation

1. **Input Validation**: Check for edge cases like empty string, invalid k values, or k greater than string length.

2. **K-mer Counting**: 
   - Iterate through all possible positions where a k-mer can start
   - Extract each k-mer using substring
   - Count occurrences using a HashMap

3. **Finding Maximum**: 
   - Determine the maximum frequency among all k-mers
   - Filter k-mers that have this maximum frequency

4. **Return Result**: Return the list of most frequent k-mers

## Time and Space Complexity

- **Time Complexity**: O(n × k) where n is the length of DNA string and k is the k-mer length
- **Space Complexity**: O(n × k) for storing the k-mers and their counts

## Sample Input/Output

**Input**: 
```
ACGTTGCATGTCGCATGATGCATGAGAGCT
4
```

**Output**: 
```
CATG GCAT
```

The solution handles multiple k-mers with the same maximum frequency and returns them in any order.

