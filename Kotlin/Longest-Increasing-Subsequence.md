```kotlin
import java.io.File

fun main() {
    val input = File("rosalind_lgis.txt").readText().trim()
    val numbers = input.lines()[1].split(" ").map { it.toInt() }
    
    val lis = longestIncreasingSubsequence(numbers)
    val lds = longestDecreasingSubsequence(numbers)
    
    println(lis.joinToString(" "))
    println(lds.joinToString(" "))
}

fun longestIncreasingSubsequence(nums: List<Int>): List<Int> {
    if (nums.isEmpty()) return emptyList()
    
    val dp = IntArray(nums.size) { 1 }
    val parent = IntArray(nums.size) { -1 }
    
    for (i in nums.indices) {
        for (j in 0 until i) {
            if (nums[i] > nums[j] && dp[i] < dp[j] + 1) {
                dp[i] = dp[j] + 1
                parent[i] = j
            }
        }
    }
    
    val maxLength = dp.maxOrNull() ?: 0
    val maxIndex = dp.indexOf(maxLength)
    
    val result = mutableListOf<Int>()
    var current = maxIndex
    
    while (current != -1) {
        result.add(0, nums[current])
        current = parent[current]
    }
    
    return result
}

fun longestDecreasingSubsequence(nums: List<Int>): List<Int> {
    if (nums.isEmpty()) return emptyList()
    
    val dp = IntArray(nums.size) { 1 }
    val parent = IntArray(nums.size) { -1 }
    
    for (i in nums.indices) {
        for (j in 0 until i) {
            if (nums[i] < nums[j] && dp[i] < dp[j] + 1) {
                dp[i] = dp[j] + 1
                parent[i] = j
            }
        }
    }
    
    val maxLength = dp.maxOrNull() ?: 0
    val maxIndex = dp.indexOf(maxLength)
    
    val result = mutableListOf<Int>()
    var current = maxIndex
    
    while (current != -1) {
        result.add(0, nums[current])
        current = parent[current]
    }
    
    return result
}
```

