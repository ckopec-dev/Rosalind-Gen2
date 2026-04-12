# Rosalind Problem: Majority Element

## Problem Description
Given an array of positive integers, find the majority element - the element that appears more than ⌊n/2⌋ times, where n is the array length.

## Solution in Go

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func majorityElement(arr []int) int {
    // Using Boyer-Moore Majority Vote Algorithm
    candidate := 0
    count := 0
    
    // Phase 1: Find candidate
    for _, num := range arr {
        if count == 0 {
            candidate = num
        }
        if num == candidate {
            count++
        } else {
            count--
        }
    }
    
    // Phase 2: Verify candidate is actually majority
    count = 0
    for _, num := range arr {
        if num == candidate {
            count++
        }
    }
    
    if count > len(arr)/2 {
        return candidate
    }
    
    return -1 // No majority element
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    
    // Read the number of arrays (if multiple test cases)
    scanner.Scan()
    t, _ := strconv.Atoi(scanner.Text())
    
    results := make([]int, t)
    
    for i := 0; i < t; i++ {
        // Read array length
        scanner.Scan()
        n, _ := strconv.Atoi(scanner.Text())
        
        // Read array elements
        scanner.Scan()
        line := scanner.Text()
        elements := strings.Fields(line)
        
        arr := make([]int, len(elements))
        for j, element := range elements {
            arr[j], _ = strconv.Atoi(element)
        }
        
        results[i] = majorityElement(arr)
    }
    
    // Print results
    for _, result := range results {
        fmt.Println(result)
    }
}
```

## Alternative Solution (Simple Counting)

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strconv"
    "strings"
)

func majorityElementSimple(arr []int) int {
    countMap := make(map[int]int)
    n := len(arr)
    
    for _, num := range arr {
        countMap[num]++
        if countMap[num] > n/2 {
            return num
        }
    }
    
    return -1
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    
    // Read array length
    scanner.Scan()
    n, _ := strconv.Atoi(scanner.Text())
    
    // Read array elements
    scanner.Scan()
    line := scanner.Text()
    elements := strings.Fields(line)
    
    arr := make([]int, len(elements))
    for i, element := range elements {
        arr[i], _ = strconv.Atoi(element)
    }
    
    result := majorityElementSimple(arr)
    fmt.Println(result)
}
```

## Input Format
```
t (number of test cases)
n (array length)
a1 a2 a3 ... an (array elements)
```

## Example Input
```
2
5
3 2 3
4
2 2 1 1
```

## Example Output
```
3
-1
```

## Explanation
- First test case: Array [3,2,3] has 3 appearing 2 times, which is > 5/2 = 2.5, so 3 is the majority element
- Second test case: Array [2,2,1,1] has no element appearing more than 4/2 = 2 times

## Time and Space Complexity
- **Boyer-Moore Algorithm**: O(n) time, O(1) space
- **Simple Counting**: O(n) time, O(n) space

The Boyer-Moore algorithm is more efficient in terms of space complexity and is the preferred approach for this problem.

