```kotlin
import java.io.File
import java.util.*

fun main() {
    val input = File("input.txt").readLines()
    val (v, w) = input
    val middleEdge = findMiddleEdge(v, w)
    println(middleEdge)
}

fun findMiddleEdge(v: String, w: String): String {
    val n = v.length
    val m = w.length
    val middle = m / 2
    
    // Calculate scores for the middle column
    val score1 = calculateScores(v, w, middle)
    val score2 = calculateScores(v, w, middle + 1)
    
    // Find the maximum score in the middle column
    var maxScore = Int.MIN_VALUE
    var maxIndex = 0
    
    for (i in 0..n) {
        val score = score1[i] - score2[i]
        if (score > maxScore) {
            maxScore = score
            maxIndex = i
        }
    }
    
    // Determine if we move right or down
    val nextIndex = if (middle + 1 < m) {
        val nextScore = calculateScores(v, w, middle + 2)
        val score = score1[maxIndex] - nextScore[maxIndex]
        if (score > 0) maxIndex else maxIndex + 1
    } else {
        maxIndex + 1
    }
    
    return "$maxIndex $nextIndex"
}

fun calculateScores(v: String, w: String, col: Int): IntArray {
    val n = v.length
    val m = w.length
    
    // Initialize DP table
    val dp = Array(n + 1) { IntArray(m + 1) { 0 } }
    
    // Fill first row
    for (j in 0..m) {
        dp[0][j] = -j
    }
    
    // Fill first column
    for (i in 0..n) {
        dp[i][0] = -i
    }
    
    // Fill the DP table up to column 'col'
    for (j in 1..col) {
        for (i in 1..n) {
            val match = if (v[i - 1] == w[j - 1]) 1 else -1
            dp[i][j] = maxOf(
                dp[i - 1][j] - 1,      // deletion
                dp[i][j - 1] - 1,      // insertion
                dp[i - 1][j - 1] + match // match/mismatch
            )
        }
    }
    
    return dp.map { it[col] }.toIntArray()
}
```

