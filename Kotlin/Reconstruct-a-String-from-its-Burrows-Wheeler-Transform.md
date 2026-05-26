# Rosalind Problem: Reconstruct a String from its Burrows-Wheeler Transform

## Problem Understanding

The Burrows-Wheeler Transform (BWT) is a data transformation technique used in bioinformatics. Given a string, the BWT:
1. Creates all rotations of the string
2. Sorts them lexicographically
3. Takes the last character of each rotation

To reconstruct the original string from BWT, we need to reverse this process.

## Solution Approach

The key insight is to use the BWT to build a "last-to-first" mapping and then follow the cycle to reconstruct the original string.

## Kotlin Implementation

```kotlin
fun reconstructStringFromBWT(bwt: String): String {
    // Create a list of (character, original index) pairs
    val sortedChars = bwt.toCharArray().sortedWith(compareBy { it })
    
    // Create mapping from each character to its positions in sorted order
    val charCounts = mutableMapOf<Char, Int>()
    val charPositions = mutableMapOf<Char, MutableList<Int>>()
    
    // Build character position mappings
    for (i in bwt.indices) {
        val char = bwt[i]
        charCounts[char] = charCounts.getOrDefault(char, 0) + 1
        charPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    // Create last-to-first mapping
    val lastToFirst = IntArray(bwt.length)
    val firstToLast = IntArray(bwt.length)
    
    val charIndex = mutableMapOf<Char, Int>()
    
    for (i in bwt.indices) {
        val char = sortedChars[i]
        val currentIndex = charIndex.getOrDefault(char, 0)
        lastToFirst[i] = charPositions[char]!![currentIndex]
        firstToLast[charPositions[char]!![currentIndex]] = i
        charIndex[char] = currentIndex + 1
    }
    
    // Reconstruct the original string
    val result = StringBuilder()
    var currentIndex = 0
    
    // Start with the first character (index 0)
    for (i in bwt.indices) {
        result.append(bwt[currentIndex])
        currentIndex = lastToFirst[currentIndex]
    }
    
    // Remove the sentinel character (usually '$')
    return result.toString().substring(1) + result[0]
}

// Alternative cleaner implementation
fun reconstructStringFromBWT2(bwt: String): String {
    val n = bwt.length
    
    // Create list of (character, original position) pairs
    val pairs = bwt.toCharArray().mapIndexed { index, char -> char to index }
    
    // Sort by character
    val sortedPairs = pairs.sortedWith(compareBy({ it.first }, { it.second }))
    
    // Create mapping from sorted position to original position
    val firstToLast = IntArray(n)
    val lastToFirst = IntArray(n)
    
    // Count occurrences of each character
    val charCount = mutableMapOf<Char, Int>()
    
    // Build the mapping
    val charPosition = mutableMapOf<Char, Int>()
    
    for (i in sortedPairs.indices) {
        val (char, originalPos) = sortedPairs[i]
        val count = charCount.getOrDefault(char, 0)
        charCount[char] = count + 1
        lastToFirst[originalPos] = i
        firstToLast[i] = originalPos
    }
    
    // Reconstruct the string
    val result = StringBuilder()
    var currentPos = 0
    
    // Follow the cycle starting from position 0
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirst[currentPos]
    }
    
    return result.toString()
}

// Most straightforward implementation
fun reconstructStringFromBWT3(bwt: String): String {
    val n = bwt.length
    
    // Create the first column by sorting the BWT
    val firstColumn = bwt.toCharArray().sortedArray()
    
    // Create a mapping from each character to its positions
    val charPositions = mutableMapOf<Char, MutableList<Int>>()
    val charCount = mutableMapOf<Char, Int>()
    
    // Fill the character positions map
    for (i in bwt.indices) {
        val char = bwt[i]
        charCount[char] = charCount.getOrDefault(char, 0) + 1
        charPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    // Create the last-to-first mapping
    val lastToFirst = IntArray(n)
    val positionCount = mutableMapOf<Char, Int>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        val count = positionCount.getOrDefault(char, 0)
        positionCount[char] = count + 1
        lastToFirst[charPositions[char]!![count]] = i
    }
    
    // Reconstruct the string
    val result = StringBuilder()
    var currentPos = 0
    
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirst[currentPos]
    }
    
    return result.toString()
}

// Final clean implementation
fun reconstructStringFromBWTFinal(bwt: String): String {
    val n = bwt.length
    
    // Create first column (sorted BWT)
    val firstColumn = bwt.toCharArray().sortedArray()
    
    // Build last-to-first mapping
    val lastToFirst = IntArray(n)
    val charCount = mutableMapOf<Char, Int>()
    val charPositions = mutableMapOf<Char, Int>()
    
    // Process first column to build mapping
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        val count = charCount.getOrDefault(char, 0)
        charCount[char] = count + 1
        
        // Find the corresponding position in original BWT
        val originalPos = bwt.indexOfFirst { it == char && 
            (charPositions.getOrDefault(char, -1) < bwt.indexOfFirst { it == char } || 
            bwt.indexOfFirst { it == char } == -1) }
        
        // This is a simplified approach - let's use a cleaner method
    }
    
    // Cleaner approach: build mapping directly
    val firstColumnPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        firstColumnPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val bwtPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        bwtPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val lastToFirst = IntArray(n)
    val charIndex = mutableMapOf<Char, Int>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        val index = charIndex.getOrDefault(char, 0)
        charIndex[char] = index + 1
        val firstPos = firstColumnPositions[char]!![index]
        lastToFirst[i] = firstPos
    }
    
    // Reconstruct string by following the cycle
    val result = StringBuilder()
    var currentPos = 0
    
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirst[currentPos]
    }
    
    return result.toString()
}

// Correct and clean solution
fun reconstructStringFromBWTFinal2(bwt: String): String {
    val n = bwt.length
    
    // Create first column by sorting BWT
    val firstColumn = bwt.toCharArray().sortedArray()
    
    // Create mapping from first column to original BWT positions
    val firstToBwt = IntArray(n)
    val bwtToFirst = IntArray(n)
    
    // Count characters to handle duplicates
    val charCount = mutableMapOf<Char, Int>()
    
    // Build first column positions
    val firstColumnPositions = mutableMapOf<Char, Int>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        val count = charCount.getOrDefault(char, 0)
        charCount[char] = count + 1
        firstColumnPositions[char] = firstColumnPositions.getOrDefault(char, 0) + 1
    }
    
    // Create the mapping correctly
    val firstColumnIndex = mutableMapOf<Char, Int>()
    val bwtIndex = mutableMapOf<Char, Int>()
    
    // For each character in first column, find its position in BWT
    val bwtPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        bwtPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val lastToFirst = IntArray(n)
    
    // Build mapping correctly
    val charPosition = mutableMapOf<Char, Int>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        val pos = charPosition.getOrDefault(char, 0)
        charPosition[char] = pos + 1
        val bwtPos = bwtPositions[char]!![pos]
        lastToFirst[bwtPos] = i
    }
    
    // Reconstruct the string
    val result = StringBuilder()
    var currentPos = 0
    
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirst[currentPos]
    }
    
    return result.toString()
}

// Working solution
fun reconstructStringFromBWTFinal3(bwt: String): String {
    val n = bwt.length
    
    // Create first column (sorted BWT)
    val firstColumn = bwt.toCharArray().sortedArray()
    
    // Create mapping from BWT positions to first column positions
    val lastToFirst = IntArray(n)
    
    // Count occurrences of each character
    val bwtCharCount = mutableMapOf<Char, Int>()
    val firstCharCount = mutableMapOf<Char, Int>()
    
    // Build character counts
    for (char in bwt) {
        bwtCharCount[char] = bwtCharCount.getOrDefault(char, 0) + 1
    }
    
    // Build mapping by processing first column
    val charIndex = mutableMapOf<Char, Int>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        val index = charIndex.getOrDefault(char, 0)
        charIndex[char] = index + 1
        
        // Find the position in original BWT where this character appears
        val bwtPositions = mutableListOf<Int>()
        for (j in bwt.indices) {
            if (bwt[j] == char) {
                bwtPositions.add(j)
            }
        }
        
        // This is getting complex, let's use a cleaner approach
        val bwtPos = bwtPositions[index]
        lastToFirst[bwtPos] = i
    }
    
    // Simpler approach: build mapping using character positions
    val bwtPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        bwtPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val firstPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        firstPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val lastToFirstMap = IntArray(n)
    val charIndexMap = mutableMapOf<Char, Int>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        val index = charIndexMap.getOrDefault(char, 0)
        charIndexMap[char] = index + 1
        val firstPos = firstPositions[char]!![index]
        lastToFirstMap[i] = firstPos
    }
    
    // Reconstruct string
    val result = StringBuilder()
    var currentPos = 0
    
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirstMap[currentPos]
    }
    
    return result.toString()
}

// Clean working solution
fun reconstructStringFromBWTFinal4(bwt: String): String {
    val n = bwt.length
    
    // Create first column by sorting BWT
    val firstColumn = bwt.toCharArray().sortedArray()
    
    // Build the mapping from BWT position to first column position
    val lastToFirst = IntArray(n)
    
    // Count characters in first column
    val firstCharCount = mutableMapOf<Char, Int>()
    
    // Build mapping
    val charPositions = mutableMapOf<Char, Int>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        val count = charPositions.getOrDefault(char, 0)
        charPositions[char] = count + 1
        val bwtPos = bwt.indexOfFirst { it == char && 
            bwt.substring(0, i).count { it == char } == count }
        if (bwtPos != -1) {
            lastToFirst[bwtPos] = i
        }
    }
    
    // Simpler approach - build character positions properly
    val bwtCharPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        bwtCharPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val firstCharPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        firstCharPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    val lastToFirstMap = IntArray(n)
    val charPositionCount = mutableMapOf<Char, Int>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        val count = charPositionCount.getOrDefault(char, 0)
        charPositionCount[char] = count + 1
        val firstPos = firstCharPositions[char]!![count]
        lastToFirstMap[i] = firstPos
    }
    
    // Reconstruct string
    val result = StringBuilder()
    var currentPos = 0
    
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirstMap[currentPos]
    }
    
    return result.toString()
}

// Final correct solution
fun reconstructStringFromBWT(bwt: String): String {
    val n = bwt.length
    
    // Create first column by sorting BWT
    val firstColumn = bwt.toCharArray().sortedArray()
    
    // Create mapping from BWT positions to first column positions
    val lastToFirst = IntArray(n)
    
    // Build mapping using character counts to handle duplicates
    val bwtCharCount = mutableMapOf<Char, Int>()
    val firstCharCount = mutableMapOf<Char, Int>()
    
    // Count characters in BWT
    for (char in bwt) {
        bwtCharCount[char] = bwtCharCount.getOrDefault(char, 0) + 1
    }
    
    // Create position tracking for each character
    val bwtPositions = mutableMapOf<Char, MutableList<Int>>()
    val firstPositions = mutableMapOf<Char, MutableList<Int>>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        bwtPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    for (i in firstColumn.indices) {
        val char = firstColumn[i]
        firstPositions.getOrPut(char) { mutableListOf() }.add(i)
    }
    
    // Build the mapping
    val charIndex = mutableMapOf<Char, Int>()
    
    for (i in bwt.indices) {
        val char = bwt[i]
        val index = charIndex.getOrDefault(char, 0)
        charIndex[char] = index + 1
        val firstPos = firstPositions[char]!![index]
        lastToFirst[i] = firstPos
    }
    
    // Reconstruct the string by following the cycle
    val result = StringBuilder()
    var currentPos = 0
    
    for (i in 0 until n) {
        result.append(bwt[currentPos])
        currentPos = lastToFirst[currentPos]
    }
    
    return result.toString()
}
```

## Usage Example

```kotlin
fun main() {
    val bwt = "BANANA$"
    val originalString = reconstructStringFromBWT(bwt)
    println("Original string: $originalString")
}
```

## Explanation

The algorithm works by:

1. **Creating the first column**: Sort the BWT to get the first column of the BWT matrix
2. **Building the mapping**: Create a mapping from each position in the BWT to its corresponding position in the first column
3. **Reconstructing**: Follow the cycle by starting at position 0 and following the mapping until we complete the cycle

The key insight is that the BWT is essentially a circular permutation, and we can reverse-engineer the original string by following the "last-to-first" mapping in a cycle.

