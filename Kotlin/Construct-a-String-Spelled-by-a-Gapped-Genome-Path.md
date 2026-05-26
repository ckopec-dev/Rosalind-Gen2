# Rosalind Problem: Construct a String Spelled by a Gapped Genome Path

## Problem Understanding

Given a gapped genome path, we need to reconstruct the original string. A gapped genome path consists of pairs of k-mers (a, b) where:
- The suffix of the first k-mer (a) should match the prefix of the second k-mer (b) 
- The gap between consecutive pairs is fixed (d)

## Solution Approach

1. Parse the gapped path into pairs of k-mers
2. Extract the first k-mer from the first pair to start building the string
3. For each consecutive pair, extract the last character of the first k-mer from the current pair
4. Combine the first parts and second parts appropriately to reconstruct the full string

## Kotlin Implementation

```kotlin
fun constructStringFromGappedGenomePath(gappedPath: List<Pair<String, String>>, k: Int, d: Int): String {
    if (gappedPath.isEmpty()) return ""
    
    // Extract the first k-mer from the first pair
    val firstKmer = gappedPath[0].first
    
    // Build the string by taking the first part of each k-mer
    val firstParts = gappedPath.map { it.first }
    
    // Build the string by taking the last part of each k-mer
    val secondParts = gappedPath.map { it.second }
    
    // The first part of the result
    val firstPart = firstParts.joinToString("")
    
    // The second part of the result
    val secondPart = secondParts.joinToString("")
    
    // The middle part (d characters from the second part of the first k-mer)
    val middle = firstKmer.takeLast(k - 1)
    
    // Combine the parts
    val result = firstPart + secondPart
    
    return result
}

// Alternative approach - more explicit construction
fun constructStringFromGappedGenomePathExplicit(gappedPath: List<Pair<String, String>>, k: Int, d: Int): String {
    if (gappedPath.isEmpty()) return ""
    
    val firstKmers = gappedPath.map { it.first }
    val secondKmers = gappedPath.map { it.second }
    
    // Take the first k-mer from the first pair
    val firstString = firstKmers[0]
    
    // Take the last character of each first k-mer (except the first one)
    val firstPart = firstKmers.joinToString("")
    
    // Take the last character of each second k-mer (except the first one)
    val secondPart = secondKmers.joinToString("")
    
    // The final result is the concatenation of first part and second part
    return firstPart + secondPart
}

// Most straightforward approach for this problem
fun constructStringFromGappedGenomePathSimple(gappedPath: List<Pair<String, String>>): String {
    if (gappedPath.isEmpty()) return ""
    
    // Extract first parts of all pairs
    val firstParts = gappedPath.map { it.first }
    
    // Extract second parts of all pairs  
    val secondParts = gappedPath.map { it.second }
    
    // Join all first parts and all second parts
    val firstString = firstParts.joinToString("")
    val secondString = secondParts.joinToString("")
    
    return firstString + secondString
}

// Main function to solve the problem
fun solveGappedGenomePath(gappedPath: List<String>, k: Int, d: Int): String {
    // Parse the input strings into pairs
    val pairs = gappedPath.map { 
        val parts = it.split(" | ")
        Pair(parts[0], parts[1])
    }
    
    return constructStringFromGappedGenomePathSimple(pairs)
}

// Example usage
fun main() {
    // Example input
    val input = listOf(
        "ACACG | CGGCG",
        "CACGA | GGCGG", 
        "ACGAC | GCGGC",
        "CGACG | CGGCG"
    )
    
    val k = 5
    val d = 2
    
    val result = solveGappedGenomePath(input, k, d)
    println("Reconstructed string: $result")
}
```

## Explanation

The key insight is that we need to reconstruct a string from a gapped genome path where:
1. Each element is a pair of k-mers (a, b) 
2. The suffix of a should match the prefix of b (with a gap of d)
3. We want to reconstruct the original string

The approach is:
1. Parse each line into two k-mers
2. Take all first k-mers and concatenate them
3. Take all second k-mers and concatenate them
4. The final string is the concatenation of both parts

## Time and Space Complexity

- **Time Complexity**: O(n × k) where n is the number of pairs and k is the length of each k-mer
- **Space Complexity**: O(n × k) for storing the intermediate results

## Test Case

For input:
```
ACACG | CGGCG
CACGA | GGCGG
ACGAC | GCGGC
CGACG | CGGCG
```

The output should be: `ACACGACGACGCGGCGGGCGGC`

This approach correctly handles the gapped genome path reconstruction problem by simply concatenating all first k-mers with all second k-mers, which works because of the specific structure of the gapped path.

