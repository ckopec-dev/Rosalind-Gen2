# Rosalind Problem: Median

## Problem Statement
Given: A positive integer n ≤ 10^5 and a array A[1..n] of integers from -10^5 to 10^5.

Return: The median of the array.

## Solution in Go

```go
package main

import (
    "fmt"
    "sort"
)

func main() {
    // Read input
    var n int
    fmt.Scanln(&n)
    
    arr := make([]int, n)
    for i := 0; i < n; i++ {
        fmt.Scan(&arr[i])
    }
    
    // Sort the array
    sort.Ints(arr)
    
    // Find median
    var median int
    if n%2 == 1 {
        // Odd length: median is the middle element
        median = arr[n/2]
    } else {
        // Even length: median is average of two middle elements
        median = (arr[n/2-1] + arr[n/2]) / 2
    }
    
    fmt.Println(median)
}
```

## Explanation

1. **Input Reading**: Read the array size `n` and then read `n` integers into an array
2. **Sorting**: Sort the array using `sort.Ints()` function
3. **Median Calculation**:
   - If array length is odd, median is the middle element at index `n/2`
   - If array length is even, median is the average of two middle elements at indices `n/2-1` and `n/2`

## Example

**Input:**
```
5
2 1 3 5 4
```

**Process:**
1. Sort array: [1, 2, 3, 4, 5]
2. Array length = 5 (odd)
3. Median = element at index 5/2 = 2 = 3

**Output:**
```
3
```

## Time and Space Complexity

- **Time Complexity**: O(n log n) due to sorting
- **Space Complexity**: O(1) additional space (sorting in-place)

## Alternative Approach

For better performance with large datasets, you could use `quickselect` algorithm to find the median in O(n) average time, but the sorting approach is simpler and sufficient for most cases.

