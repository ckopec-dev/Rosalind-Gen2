```kotlin
import java.io.File

fun main() {
    val input = File("input.txt").readText().trim()
    val lines = input.split("\n")
    val str1 = lines[0]
    val str2 = lines[1]
    
    val result = findHighestScoringLocalAlignment(str1, str2)
    println(result.first)
    println(result.second)
}

fun findHighestScoringLocalAlignment(str1: String, str2: String): Pair<Int, String> {
    val n = str1.length
    val m = str2.length
    
    // Scoring matrix
    val scoreMatrix = Array(n + 1) { Array(m + 1) { 0 } }
    
    // Backtracking matrix to reconstruct alignment
    val backtrackMatrix = Array(n + 1) { Array(m + 1) { 0 } }
    
    // Initialize scoring matrix
    for (i in 1..n) {
        scoreMatrix[i][0] = 0
    }
    
    for (j in 1..m) {
        scoreMatrix[0][j] = 0
    }
    
    // Fill the scoring matrix
    var maxScore = 0
    var maxI = 0
    var maxJ = 0
    
    for (i in 1..n) {
        for (j in 1..m) {
            val match = if (str1[i - 1] == str2[j - 1]) 1 else -1
            val diagonal = scoreMatrix[i - 1][j - 1] + match
            val up = scoreMatrix[i - 1][j] - 2
            val left = scoreMatrix[i][j - 1] - 2
            val currentScore = maxOf(0, diagonal, up, left)
            
            scoreMatrix[i][j] = currentScore
            
            if (currentScore > maxScore) {
                maxScore = currentScore
                maxI = i
                maxJ = j
            }
            
            // Determine direction for backtracking
            backtrackMatrix[i][j] = when (currentScore) {
                diagonal -> 1 // diagonal
                up -> 2 // up
                left -> 3 // left
                else -> 0 // 0 means reset (score was 0)
            }
        }
    }
    
    // Reconstruct the alignment
    val alignment1 = StringBuilder()
    val alignment2 = StringBuilder()
    
    var i = maxI
    var j = maxJ
    
    while (i > 0 && j > 0 && scoreMatrix[i][j] > 0) {
        when (backtrackMatrix[i][j]) {
            1 -> { // diagonal
                alignment1.insert(0, str1[i - 1])
                alignment2.insert(0, str2[j - 1])
                i--
                j--
            }
            2 -> { // up
                alignment1.insert(0, str1[i - 1])
                alignment2.insert(0, '-')
                i--
            }
            3 -> { // left
                alignment1.insert(0, '-')
                alignment2.insert(0, str2[j - 1])
                j--
            }
        }
    }
    
    return Pair(maxScore, "${alignment1}\n${alignment2}")
}
```

