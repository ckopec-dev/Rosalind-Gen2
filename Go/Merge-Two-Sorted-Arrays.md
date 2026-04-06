# Rosalind Problem: Merge Two Sorted Arrays

## Problem Description
Given two sorted arrays A and B of length n each, merge them into a single sorted array.

## Solution in Go

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

func mergeArrays(a, b []int) []int {
    // Initialize result array with combined length
    result := make([]int, len(a)+len(b))
    
    // Initialize pointers for both arrays
    i, j, k := 0, 0, 0
    
    // Merge elements while both arrays have elements
    for i < len(a) && j < len(b) {
        if a[i] <= b[j] {
            result[k] = a[i]
            i++
        } else {
            result[k] = b[j]
            j++
        }
        k++
    }
    
    // Copy remaining elements from array a (if any)
    for i < len(a) {
        result[k] = a[i]
        i++
        k++
    }
    
    // Copy remaining elements from array b (if any)
    for j < len(b) {
        result[k] = b[j]
        j++
        k++
    }
    
    return result
}

func main() {
    // Read input from stdin
    var input string
    fmt.Scanln(&input)
    
    // Read first array
    fmt.Scanln(&input)
    arr1 := parseArray(input)
    
    // Read second array
    fmt.Scanln(&input)
    arr2 := parseArray(input)
    
    // Merge arrays
    merged := mergeArrays(arr1, arr2)
    
    // Print result
    fmt.Println(strings.Trim(strings.Replace(fmt.Sprint(merged), " ", " ", -1), "[]"))
}

func parseArray(input string) []int {
    // Remove brackets if present
    input = strings.TrimSpace(input)
    if strings.HasPrefix(input, "[") && strings.HasSuffix(input, "]") {
        input = input[1 : len(input)-1]
    }
    
    // Split by spaces and convert to integers
    parts := strings.Fields(input)
    result := make([]int, len(parts))
    
    for i, part := range parts {
        num, _ := strconv.Atoi(part)
        result[i] = num
    }
    
    return result
}
```

## Alternative Simpler Solution

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

func mergeArrays(a, b []int) []int {
    result := make([]int, 0, len(a)+len(b))
    i, j := 0, 0
    
    // Compare and merge elements
    for i < len(a) && j < len(b) {
        if a[i] <= b[j] {
            result = append(result, a[i])
            i++
        } else {
            result = append(result, b[j])
            j++
        }
    }
    
    // Add remaining elements
    for i < len(a) {
        result = append(result, a[i])
        i++
    }
    
    for j < len(b) {
        result = append(result, b[j])
        j++
    }
    
    return result
}

func main() {
    // Read input
    var n int
    fmt.Scanln(&n)
    
    // Read first array
    arr1 := make([]int, n)
    for i := 0; i < n; i++ {
        fmt.Scan(&arr1[i])
    }
    
    // Read second array
    arr2 := make([]int, n)
    for i := 0; i < n; i++ {
        fmt.Scan(&arr2[i])
    }
    
    // Merge and print result
    merged := mergeArrays(arr1, arr2)
    for i, val := range merged {
        if i > 0 {
            fmt.Print(" ")
        }
        fmt.Print(val)
    }
    fmt.Println()
}
```

## Example Usage

**Input:**
```
3
1 3 5
2 4 6
```

**Output:**
```
1 2 3 4 5 6
```

## Algorithm Explanation

1. **Two-pointer approach**: Use two pointers to traverse both arrays simultaneously
2. **Compare elements**: At each step, compare the current elements of both arrays
3. **Merge smaller element**: Add the smaller element to result and advance that pointer
4. **Handle remaining elements**: After one array is exhausted, add all remaining elements from the other array
5. **Time complexity**: O(n + m) where n and m are the lengths of the arrays
6. **Space complexity**: O(n + m) for the result array

The solution handles the merging of two sorted arrays efficiently in linear time, which is optimal for this problem.

