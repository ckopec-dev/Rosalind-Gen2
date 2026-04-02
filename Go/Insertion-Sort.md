# Rosalind Problem: Insertion Sort Solution in Go

## Problem Understanding

The insertion sort problem requires us to sort an array of integers using the insertion sort algorithm and return the number of swaps performed during the sorting process.

## Solution

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

// insertionSort counts the number of swaps needed to sort an array using insertion sort
func insertionSort(arr []int) int {
    swaps := 0
    n := len(arr)
    
    // Start from the second element (index 1)
    for i := 1; i < n; i++ {
        key := arr[i]
        j := i - 1
        
        // Move elements greater than key one position ahead
        for j >= 0 && arr[j] > key {
            arr[j+1] = arr[j]
            j--
            swaps++
        }
        
        // Place key at its correct position
        arr[j+1] = key
    }
    
    return swaps
}

// parseInput parses the input string into an array of integers
func parseInput(input string) []int {
    parts := strings.Fields(input)
    arr := make([]int, len(parts))
    
    for i, part := range parts {
        num, _ := strconv.Atoi(part)
        arr[i] = num
    }
    
    return arr
}

func main() {
    // Read input from stdin
    var input string
    fmt.Scanln(&input)
    
    // Parse the input to get the array
    arr := parseInput(input)
    
    // Perform insertion sort and get the number of swaps
    swaps := insertionSort(arr)
    
    // Print the result
    fmt.Println(swaps)
}
```

## How it works:

1. **Algorithm**: The insertion sort algorithm works by building the final sorted array one element at a time. It takes each element and inserts it into its correct position among the already sorted elements.

2. **Counting Swaps**: Each time we shift an element to make space for the key element, we increment our swap counter.

3. **Process**:
   - Start from the second element (index 1)
   - Take the current element as the key
   - Compare the key with elements to its left
   - Shift larger elements one position to the right
   - Insert the key in its correct position
   - Count each shift operation as one swap

## Example:

For input array `[4, 3, 2, 1]`:
- Compare 3 with 4, shift 4 → 1 swap
- Compare 2 with 3, shift 3 → 1 swap
- Compare 2 with 4, shift 4 → 1 swap
- Compare 1 with 2, shift 2 → 1 swap
- Compare 1 with 3, shift 3 → 1 swap
- Compare 1 with 4, shift 4 → 1 swap
- Total: 6 swaps

## Time and Space Complexity:

- **Time Complexity**: O(n²) in worst case, O(n) in best case
- **Space Complexity**: O(1) - sorts in place

## Usage:

To run the program:
```bash
go run insertion_sort.go < input.txt
```

Where `input.txt` contains the array elements separated by spaces.

