```kotlin
import java.io.File

fun main() {
    val input = File("input.txt").readLines()
    val str1 = input[0]
    val str2 = input[1]
    
    val (alignment1, alignment2) = alignTwoStringsLinearSpace(str1, str2)
    println(alignment1)
    println(alignment2)
}

fun alignTwoStringsLinearSpace(str1: String, str2: String): Pair<String, String> {
    val n = str1.length
    val m = str2.length
    
    // Initialize the score matrix with only two rows (linear space)
    val prev = IntArray(m + 1) { it }
    val curr = IntArray(m + 1) { 0 }
    
    // Fill the first row
    for (j in 0..m) {
        prev[j] = j
    }
    
    // Fill the matrix using only two rows
    for (i in 1..n) {
        curr[0] = i
        for (j in 1..m) {
            val match = if (str1[i - 1] == str2[j - 1]) 0 else 1
            curr[j] = minOf(
                prev[j] + 1,        // deletion
                curr[j - 1] + 1,    // insertion
                prev[j - 1] + match // match/mismatch
            )
        }
        // Swap arrays
        val temp = prev
        prev.fill(curr)
        curr.fill(0)
    }
    
    // Backtrack to find the alignment
    val alignment1 = StringBuilder()
    val alignment2 = StringBuilder()
    
    var i = n
    var j = m
    
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && 
            prev[j] == prev[j - 1] + 1 && 
            curr[j] == prev[j - 1] + 1) {
            // Match/mismatch
            alignment1.insert(0, str1[i - 1])
            alignment2.insert(0, str2[j - 1])
            i--
            j--
        } else if (i > 0 && curr[j] == prev[j] + 1) {
            // Deletion
            alignment1.insert(0, str1[i - 1])
            alignment2.insert(0, '-')
            i--
        } else if (j > 0 && curr[j] == curr[j - 1] + 1) {
            // Insertion
            alignment1.insert(0, '-')
            alignment2.insert(0, str2[j - 1])
            j--
        } else {
            // This shouldn't happen in normal cases
            alignment1.insert(0, str1[i - 1])
            alignment2.insert(0, str2[j - 1])
            i--
            j--
        }
    }
    
    return Pair(alignment1.toString(), alignment2.toString())
}

// Alternative implementation with proper backtracking
fun alignTwoStringsLinearSpaceProper(str1: String, str2: String): Pair<String, String> {
    val n = str1.length
    val m = str2.length
    
    // Create a matrix to store the minimum edit distance
    val dp = Array(n + 1) { IntArray(m + 1) }
    
    // Initialize base cases
    for (i in 0..n) dp[i][0] = i
    for (j in 0..m) dp[0][j] = j
    
    // Fill the matrix
    for (i in 1..n) {
        for (j in 1..m) {
            val match = if (str1[i - 1] == str2[j - 1]) 0 else 1
            dp[i][j] = minOf(
                dp[i - 1][j] + 1,      // deletion
                dp[i][j - 1] + 1,      // insertion
                dp[i - 1][j - 1] + match // match/mismatch
            )
        }
    }
    
    // Backtrack to find the alignment
    val alignment1 = StringBuilder()
    val alignment2 = StringBuilder()
    
    var i = n
    var j = m
    
    while (i > 0 || j > 0) {
        if (i > 0 && j > 0 && dp[i][j] == dp[i - 1][j - 1] + 
            if (str1[i - 1] == str2[j - 1]) 0 else 1) {
            // Match/mismatch
            alignment1.insert(0, str1[i - 1])
            alignment2.insert(0, str2[j - 1])
            i--
            j--
        } else if (i > 0 && dp[i][j] == dp[i - 1][j] + 1) {
            // Deletion
            alignment1.insert(0, str1[i - 1])
            alignment2.insert(0, '-')
            i--
        } else if (j > 0 && dp[i][j] == dp[i][j - 1] + 1) {
            // Insertion
            alignment1.insert(0, '-')
            alignment2.insert(0, str2[j - 1])
            j--
        }
    }
    
    return Pair(alignment1.toString(), alignment2.toString())
}
```

This solution implements the linear space alignment algorithm for two strings. The approach uses dynamic programming with space optimization by only maintaining two rows of the DP matrix instead of the full matrix.

Key aspects of the implementation:

1. **Space Optimization**: Instead of creating an n×m matrix, we only maintain two arrays (`prev` and `curr`) of size m+1, reducing space complexity from O(n×m) to O(m).

2. **Dynamic Programming**: We compute the minimum edit distance using the standard recurrence relation:
   - `dp[i][j] = min(dp[i-1][j] + 1, dp[i][j-1] + 1, dp[i-1][j-1] + match)`
   - Where match is 0 if characters are equal, 1 otherwise

3. **Backtracking**: After computing the edit distance, we trace back through the matrix to construct the actual alignment strings.

4. **Time Complexity**: O(n×m) time, O(m) space

The algorithm handles three operations:
- **Insertion**: Add a gap in the first string
- **Deletion**: Remove a character from the first string  
- **Substitution**: Replace a character in the first string with another

The output consists of two aligned strings where gaps are represented by '-' characters.

