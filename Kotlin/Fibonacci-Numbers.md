# Rosalind Problem: Fibonacci Numbers (Kotlin Solution)

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of Fn, where Fn is the n-th Fibonacci number.

## Solution

```kotlin
fun fibonacci(n: Int): Long {
    if (n <= 2) return 1L
    
    var prev2 = 1L
    var prev1 = 1L
    var current = 0L
    
    for (i in 3..n) {
        current = prev1 + prev2
        prev2 = prev1
        prev1 = current
    }
    
    return current
}

fun main() {
    val n = readLine()!!.toInt()
    println(fibonacci(n))
}
```

## Explanation

This solution implements an efficient iterative approach to calculate the n-th Fibonacci number:

1. **Base cases**: For n ≤ 2, the Fibonacci number is 1
2. **Iterative calculation**: Starting from the 3rd number, we calculate each Fibonacci number by summing the two previous numbers
3. **Space optimization**: We only keep track of the last two numbers instead of storing the entire sequence
4. **Time complexity**: O(n)
5. **Space complexity**: O(1)

## Example
For input n = 5:
- F₁ = 1
- F₂ = 1  
- F₃ = 1 + 1 = 2
- F₄ = 1 + 2 = 3
- F₅ = 2 + 3 = 5

Output: 5

## Alternative Recursive Solution (Less Efficient)

```kotlin
fun fibonacciRecursive(n: Int): Long {
    if (n <= 2) return 1L
    return fibonacciRecursive(n - 1) + fibonacciRecursive(n - 2)
}
```

The iterative solution is preferred for this problem due to its efficiency and ability to handle the constraint n ≤ 25 without stack overflow issues.

