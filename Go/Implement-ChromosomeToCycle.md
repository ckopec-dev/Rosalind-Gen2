# Rosalind Problem: Implement_ChromosomeToCycle in Go

## Problem Description
Convert a chromosome (a sequence of signed integers) into a cycle representation.

In genome rearrangement problems, we often need to represent chromosomes as cycles. Given a chromosome represented as a sequence of signed integers, we want to convert it into a cycle where each element is represented as two nodes (one for the head and one for the tail of the element).

## Solution

```go
package main

import (
    "fmt"
    "strconv"
    "strings"
)

// ChromosomeToCycle converts a chromosome (sequence of signed integers) 
// into a cycle representation
func ChromosomeToCycle(chromosome []int) []int {
    cycle := make([]int, len(chromosome)*2)
    
    for i, element := range chromosome {
        if element > 0 {
            // For positive elements: 2*i+1 -> 2*i+2
            cycle[2*i] = 2*element - 1   // Head of element
            cycle[2*i+1] = 2*element     // Tail of element
        } else {
            // For negative elements: 2*i+2 -> 2*i+1
            cycle[2*i] = 2*(-element)    // Head of element
            cycle[2*i+1] = 2*(-element) - 1 // Tail of element
        }
    }
    
    return cycle
}

// Helper function to format output as required by Rosalind
func FormatCycle(cycle []int) string {
    var result []string
    for _, num := range cycle {
        result = append(result, strconv.Itoa(num))
    }
    return strings.Join(result, " ")
}

func main() {
    // Example from Rosalind problem
    chromosome := []int{1, -2, 3, 4, -5}
    cycle := ChromosomeToCycle(chromosome)
    
    fmt.Printf("Input chromosome: %v\n", chromosome)
    fmt.Printf("Output cycle: %s\n", FormatCycle(cycle))
    
    // Test with another example
    chromosome2 := []int{-1, 2, -3}
    cycle2 := ChromosomeToCycle(chromosome2)
    
    fmt.Printf("Input chromosome: %v\n", chromosome2)
    fmt.Printf("Output cycle: %s\n", FormatCycle(cycle2))
}
```

## Explanation

The algorithm works as follows:

1. **Input**: A chromosome represented as a sequence of signed integers (e.g., `[1, -2, 3, 4, -5]`)

2. **Process**: 
   - For each element in the chromosome, we create two nodes:
     - For positive elements: if element = k, we create nodes `2k-1` and `2k`
     - For negative elements: if element = -k, we create nodes `2k` and `2k-1`
   - These nodes are arranged in a cycle where each element is represented by two consecutive nodes

3. **Output**: A cycle represented as a sequence of integers

## Example Walkthrough

For chromosome `[1, -2, 3, 4, -5]`:
- Element 1 (positive): creates nodes 1 and 2
- Element -2 (negative): creates nodes 4 and 3  
- Element 3 (positive): creates nodes 5 and 6
- Element 4 (positive): creates nodes 7 and 8
- Element -5 (negative): creates nodes 10 and 9

Result: `[1, 2, 4, 3, 5, 6, 7, 8, 10, 9]`

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the chromosome
- **Space Complexity**: O(n) for the output cycle array

This implementation correctly handles both positive and negative integers and produces the proper cycle representation needed for further genome rearrangement algorithms.

