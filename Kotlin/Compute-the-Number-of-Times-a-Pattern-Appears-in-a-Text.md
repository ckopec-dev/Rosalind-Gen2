# Rosalind Problem: Compute the Number of Times a Pattern Appears in a Text

## Problem Description
Given two strings `pattern` and `text`, we need to count how many times `pattern` appears in `text` as a substring, including overlapping occurrences.

## Solution in Kotlin

```kotlin
fun countPatternOccurrences(pattern: String, text: String): Int {
    if (pattern.isEmpty() || text.isEmpty() || pattern.length > text.length) {
        return 0
    }
    
    var count = 0
    val patternLength = pattern.length
    val textLength = text.length
    
    // Iterate through each possible starting position in text
    for (i in 0..textLength - patternLength) {
        // Check if pattern matches at position i
        if (text.substring(i, i + patternLength) == pattern) {
            count++
        }
    }
    
    return count
}

fun main() {
    // Example usage
    val pattern = "GATATATGCATATACTT"
    val text = "GATATATGCATATACTT"
    
    val result = countPatternOccurrences(pattern, text)
    println("Pattern '$pattern' appears $result times in text")
    
    // Test with overlapping patterns
    val pattern2 = "ATA"
    val text2 = "ATATAT"
    val result2 = countPatternOccurrences(pattern2, text2)
    println("Pattern '$pattern2' appears $result2 times in text '$text2'")
}
```

## Alternative Implementation (More Efficient)

```kotlin
fun countPatternOccurrencesOptimized(pattern: String, text: String): Int {
    if (pattern.isEmpty() || text.isEmpty() || pattern.length > text.length) {
        return 0
    }
    
    var count = 0
    val patternLength = pattern.length
    
    // Use indexOf to find all occurrences
    var startIndex = 0
    while (startIndex <= text.length - pattern.length) {
        val foundIndex = text.indexOf(pattern, startIndex)
        if (foundIndex == -1) {
            break
        }
        count++
        startIndex = foundIndex + 1 // Move by 1 to catch overlapping patterns
    }
    
    return count
}

fun main() {
    // Example from Rosalind
    val pattern = "GATATATGCATATACTT"
    val text = "GATATATGCATATACTT"
    
    val result = countPatternOccurrencesOptimized(pattern, text)
    println(result)
}
```

## Explanation

1. **Input Validation**: First, we check if the pattern is empty, text is empty, or if the pattern is longer than the text.

2. **Main Logic**: We iterate through each possible starting position in the text where the pattern could fit.

3. **Pattern Matching**: At each position, we extract a substring of the same length as the pattern and compare it with the pattern.

4. **Overlapping Handling**: Since we increment by 1 each time (not by pattern length), we automatically capture overlapping occurrences.

5. **Edge Cases**: The solution handles empty strings and patterns longer than the text gracefully.

## Time Complexity
- **Time**: O(n × m) where n is the length of text and m is the length of pattern
- **Space**: O(1) additional space

## Sample Input/Output
```
Input:
pattern = "ATA"
text = "ATATAT"

Output: 3

Explanation: 
- Position 0: "ATA" matches
- Position 1: "TAT" doesn't match
- Position 2: "ATA" matches  
- Position 3: "TAT" doesn't match
- Position 4: "ATA" matches
```

The solution correctly handles overlapping patterns and provides an efficient way to count pattern occurrences in a text string.

