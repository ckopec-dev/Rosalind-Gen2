# Rosalind Problem: Fibonacci Numbers (FIB)

## Problem Statement
Given: A positive integer n ≤ 1000.

Return: The total number of rabbit pairs that will be present after n months, if we begin with 1 pair and in each generation, every pair breeds a pair of offspring (which becomes reproductive the next month).

This is the classic Fibonacci sequence where:
- F(1) = 1
- F(2) = 1  
- F(n) = F(n-1) + F(n-2) for n > 2

## Solution in Go

```go
package main

import (
    "fmt"
    "os"
    "strconv"
)

func fibonacci(n int) int {
    if n <= 0 {
        return 0
    }
    if n == 1 || n == 2 {
        return 1
    }
    
    // Use iterative approach to avoid stack overflow for large n
    a, b := 1, 1
    for i := 3; i <= n; i++ {
        a, b = b, a+b
    }
    return b
}

func main() {
    // Read input from command line argument
    if len(os.Args) != 2 {
        fmt.Println("Usage: go run main.go <n>")
        os.Exit(1)
    }
    
    n, err := strconv.Atoi(os.Args[1])
    if err != nil {
        fmt.Println("Error: Please provide a valid integer")
        os.Exit(1)
    }
    
    if n <= 0 || n > 1000 {
        fmt.Println("Error: n must be a positive integer between 1 and 1000")
        os.Exit(1)
    }
    
    result := fibonacci(n)
    fmt.Println(result)
}
```

## Alternative Solution (More Memory Efficient)

```go
package main

import (
    "fmt"
    "os"
    "strconv"
)

func fibonacci(n int) int {
    if n <= 0 {
        return 0
    }
    if n == 1 || n == 2 {
        return 1
    }
    
    // Use only two variables to store previous values
    prev2 := 1  // F(n-2)
    prev1 := 1  // F(n-1)
    current := 0
    
    for i := 3; i <= n; i++ {
        current = prev1 + prev2
        prev2 = prev1
        prev1 = current
    }
    
    return current
}

func main() {
    if len(os.Args) != 2 {
        fmt.Println("Usage: go run main.go <n>")
        os.Exit(1)
    }
    
    n, err := strconv.Atoi(os.Args[1])
    if err != nil {
        fmt.Println("Error: Please provide a valid integer")
        os.Exit(1)
    }
    
    if n <= 0 || n > 1000 {
        fmt.Println("Error: n must be a positive integer between 1 and 1000")
        os.Exit(1)
    }
    
    result := fibonacci(n)
    fmt.Println(result)
}
```

## How to Run

1. Save the code in a file named `main.go`
2. Run with a command line argument:
```bash
go run main.go 5
```

## Example

**Input:** 5
**Output:** 5

**Explanation:**
- Month 1: 1 pair
- Month 2: 1 pair  
- Month 3: 2 pairs (1 original + 1 new)
- Month 4: 3 pairs (2 previous + 1 new)
- Month 5: 5 pairs (3 previous + 2 new)

The sequence follows: 1, 1, 2, 3, 5, 8, 13, ...

## Time and Space Complexity

- **Time Complexity:** O(n) - single loop through n months
- **Space Complexity:** O(1) - only using constant extra space

This solution efficiently handles the constraint that n can be up to 1000 without any performance issues.

