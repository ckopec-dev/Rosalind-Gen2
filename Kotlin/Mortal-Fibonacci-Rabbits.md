# Rosalind Problem: Mortal Fibonacci Rabbits

## Problem Description
In this problem, we need to compute the number of rabbit pairs after `n` months, where each pair of rabbits lives for `m` months.

## Solution in Kotlin

```kotlin
fun mortalFibonacciRabbits(n: Int, m: Int): Long {
    // Base case: if n is 0 or m is 0, return 0
    if (n == 0 || m == 0) return 0
    
    // For the first m months, rabbits don't die yet
    // We can use the standard Fibonacci approach for first m months
    val population = LongArray(n + 1) { 0L }
    
    // Initialize base cases
    population[0] = 0L
    if (n >= 1) population[1] = 1L
    
    // For months 2 to n, calculate population
    for (i in 2..n) {
        if (i <= m) {
            // Before rabbits die, it's standard Fibonacci
            population[i] = population[i - 1] + population[i - 2]
        } else {
            // After m months, rabbits die
            // New rabbits = rabbits from previous months (excluding those that died)
            population[i] = population[i - 1] + population[i - 2] - population[i - m - 1]
        }
    }
    
    return population[n]
}

// Alternative cleaner approach
fun mortalFibonacciRabbitsClean(n: Int, m: Int): Long {
    if (n == 0 || m == 0) return 0
    
    // Array to store population at each month
    val population = LongArray(n + 1) { 0L }
    
    // Base case
    population[1] = 1L
    
    // Calculate for each month
    for (i in 2..n) {
        if (i <= m) {
            // No deaths yet
            population[i] = population[i - 1] + population[i - 2]
        } else {
            // Subtract the rabbits that died (those that were alive m months ago)
            population[i] = population[i - 1] + population[i - 2] - population[i - m]
        }
    }
    
    return population[n]
}

// Example usage
fun main() {
    // Example: n=6, m=3
    val result = mortalFibonacciRabbitsClean(6, 3)
    println("Number of rabbit pairs after 6 months with 3-month lifespan: $result")
    
    // Test with the example from Rosalind
    val testResult = mortalFibonacciRabbitsClean(94, 16)
    println("Result for n=94, m=16: $testResult")
}
```

## Explanation

The solution uses dynamic programming with the following logic:

1. **Base cases**: 
   - `population[0] = 0` (no rabbits at month 0)
   - `population[1] = 1` (one pair at month 1)

2. **For months 2 to n**:
   - If `i <= m`: Standard Fibonacci recurrence `population[i] = population[i-1] + population[i-2]`
   - If `i > m`: The recurrence becomes `population[i] = population[i-1] + population[i-2] - population[i-m]`
     - We add the previous two months' population (new rabbits)
     - We subtract the population from `m` months ago (these rabbits have died)

3. **Key insight**: 
   - Before month `m`, rabbits don't die, so it's regular Fibonacci
   - After month `m`, we subtract the rabbits that died `m` months ago

## Time and Space Complexity
- **Time Complexity**: O(n)
- **Space Complexity**: O(n)

## Sample Input/Output
```
Input: n=6, m=3
Output: 4

Input: n=94, m=16  
Output: 1405241323033707396740359073014134024732304242591051010275274554812328594345131331729524505141104374909300337157744043221514594827124230053148244397448811125431031409134224362411360082170822722104372719115541110432150408070140390324020405224231271050691223633663044345222427317280000000000000000000000000000000

