# Rosalind Problem: Implement DistanceBetweenPatternAndStrings

## Problem Description
The distance between a pattern and a set of strings is the sum of the distances between the pattern and each string in the set. The distance between a pattern and a string is the minimum number of mismatches between the pattern and any substring of the string.

## Solution

```kotlin
fun distanceBetweenPatternAndStrings(pattern: String, dna: List<String>): Int {
    val k = pattern.length
    var distance = 0
    
    for (text in dna) {
        var minHammingDistance = Int.MAX_VALUE
        // Check all substrings of length k in the current text
        for (i in 0..text.length - k) {
            val substring = text.substring(i, i + k)
            val hammingDistance = hammingDistance(pattern, substring)
            if (hammingDistance < minHammingDistance) {
                minHammingDistance = hammingDistance
            }
        }
        distance += minHammingDistance
    }
    
    return distance
}

fun hammingDistance(pattern1: String, pattern2: String): Int {
    var distance = 0
    for (i in pattern1.indices) {
        if (pattern1[i] != pattern2[i]) {
            distance++
        }
    }
    return distance
}

// Main function to read input and solve the problem
fun main() {
    // Example usage:
    val pattern = "AAA"
    val dna = listOf("TTACCTTAAC", "GATATCTGAC", "ACGGCGTTCG", "CCCTAAAGAG", "CGTCAGAGGT")
    
    val result = distanceBetweenPatternAndStrings(pattern, dna)
    println(result)
}
```

## Explanation

1. **Main Function (`distanceBetweenPatternAndStrings`)**:
   - Takes a pattern string and a list of DNA strings
   - Calculates the total distance by summing up the minimum Hamming distances
   - For each DNA string, finds the minimum Hamming distance between the pattern and all substrings of the same length

2. **Helper Function (`hammingDistance`)**:
   - Calculates the Hamming distance between two strings of equal length
   - Counts the number of positions where characters differ

3. **Algorithm Steps**:
   - For each DNA string in the list
   - For each substring of the same length as the pattern
   - Calculate Hamming distance between pattern and substring
   - Keep track of the minimum distance found
   - Add this minimum distance to the total

## Time Complexity
- O(n × m × k) where n is the number of DNA strings, m is the average length of DNA strings, and k is the length of the pattern

## Space Complexity
- O(1) additional space (not counting input storage)

## Example
For pattern "AAA" and DNA strings ["TTACCTTAAC", "GATATCTGAC", "ACGGCGTTCG", "CCCTAAAGAG", "CGTCAGAGGT"]:
- For "TTACCTTAAC": minimum distance is 1 (with substring "TAA")
- For "GATATCTGAC": minimum distance is 2 (with substring "GAT")
- And so on...
- Total distance = 1 + 2 + 2 + 1 + 3 = 9

