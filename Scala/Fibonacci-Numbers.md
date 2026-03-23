# Rosalind Problem: Fibonacci Numbers - Scala Solution

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of Fn (the nth Fibonacci number).

## Solution

```scala
def fibonacci(n: Int): Long = {
  if (n <= 2) 1
  else {
    var a = 1L
    var b = 1L
    var i = 2
    while (i < n) {
      val temp = a + b
      a = b
      b = temp
      i += 1
    }
    b
  }
}

// Alternative recursive solution (less efficient for large n)
def fibonacciRecursive(n: Int): Long = {
  if (n <= 2) 1
  else fibonacciRecursive(n - 1) + fibonacciRecursive(n - 2)
}

// More efficient recursive with memoization
def fibonacciMemo(n: Int): Long = {
  val memo = scala.collection.mutable.Map[Int, Long]()
  
  def fibHelper(n: Int): Long = {
    if (n <= 2) 1
    else if (memo.contains(n)) memo(n)
    else {
      val result = fibHelper(n - 1) + fibHelper(n - 2)
      memo(n) = result
      result
    }
  }
  
  fibHelper(n)
}

// Read input and solve
val n = io.StdIn.readLine().toInt
println(fibonacci(n))
```

## Explanation

The Fibonacci sequence is defined as:
- F₁ = 1
- F₂ = 1  
- Fₙ = Fₙ₋₁ + Fₙ₋₂ for n > 2

I've provided three approaches:

1. **Iterative approach** (`fibonacci`): Most efficient, O(n) time and O(1) space
2. **Simple recursive** (`fibonacciRecursive`): Easy to understand but exponential time complexity
3. **Memoized recursive** (`fibonacciMemo`): Better recursive approach with O(n) time and O(n) space

For the Rosalind problem with n ≤ 25, any approach will work fine, but the iterative solution is recommended for its efficiency.

## Sample Input/Output
```
Input: 5
Output: 5

Input: 10
Output: 55
```

The iterative solution is preferred for this problem as it's efficient and handles the constraint well.

