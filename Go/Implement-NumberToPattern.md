# Rosalind Problem: Implement_NumberToPattern

## Problem Description
Implement the NumberToPattern function that converts a number to its corresponding DNA pattern of a given length.

## Solution

```go
package main

import (
    "fmt"
    "math"
)

// NumberToPattern converts a number to its corresponding DNA pattern of given length
func NumberToPattern(index int, k int) string {
    if k == 1 {
        return NumberToSymbol(index)
    }
    
    prefixIndex := index / 4
    remainder := index % 4
    symbol := NumberToSymbol(remainder)
    prefixPattern := NumberToPattern(prefixIndex, k-1)
    
    return prefixPattern + symbol
}

// NumberToSymbol converts a number (0-3) to corresponding DNA symbol
func NumberToSymbol(index int) string {
    switch index {
    case 0:
        return "A"
    case 1:
        return "C"
    case 2:
        return "G"
    case 3:
        return "T"
    default:
        return ""
    }
}

// Alternative iterative implementation
func NumberToPatternIterative(index int, k int) string {
    pattern := make([]byte, k)
    
    for i := k - 1; i >= 0; i-- {
        remainder := index % 4
        pattern[i] = byte(NumberToSymbol(remainder)[0])
        index = index / 4
    }
    
    return string(pattern)
}

func main() {
    // Test cases
    fmt.Println(NumberToPattern(45, 4))  // Expected: "ACGT"
    fmt.Println(NumberToPattern(11, 2))  // Expected: "CC"
    fmt.Println(NumberToPattern(0, 1))   // Expected: "A"
    
    // Test iterative version
    fmt.Println(NumberToPatternIterative(45, 4))  // Expected: "ACGT"
    fmt.Println(NumberToPatternIterative(11, 2))  // Expected: "CC"
}
```

## Explanation

### Approach
This problem uses a recursive approach based on the concept of base-4 number system where:
- 0 → 'A'
- 1 → 'C' 
- 2 → 'G'
- 3 → 'T'

### Recursive Solution
1. **Base case**: If k=1, return the DNA symbol corresponding to the number
2. **Recursive case**: 
   - Calculate prefix index: `index / 4`
   - Calculate remainder: `index % 4`
   - Get the symbol for remainder
   - Recursively get the pattern for the prefix index with length k-1
   - Concatenate prefix pattern and symbol

### Iterative Solution
1. Create a character array of size k
2. Starting from the last position, calculate each symbol by:
   - Finding remainder of index divided by 4
   - Converting remainder to DNA symbol
   - Updating index to index divided by 4
3. Return the constructed string

### Time Complexity
- **Recursive**: O(k) where k is the pattern length
- **Iterative**: O(k) where k is the pattern length

### Space Complexity
- **Recursive**: O(k) due to recursion stack
- **Iterative**: O(k) for the result string

The recursive solution is more intuitive and directly follows the mathematical relationship, while the iterative solution avoids potential stack overflow for very large inputs.

