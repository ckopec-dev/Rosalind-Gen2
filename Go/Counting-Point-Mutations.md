# Rosalind Problem: Counting Point Mutations (BA1G)

## Problem Statement
Given two strings s and t of equal length, count the number of positions where the characters differ (Hamming distance).

## Solution in Go

```go
package main

import (
    "fmt"
    "io/ioutil"
    "strings"
)

// CountPointMutations counts the number of point mutations between two DNA strings
func CountPointMutations(s, t string) int {
    if len(s) != len(t) {
        return -1 // Error: strings must be of equal length
    }
    
    mutations := 0
    for i := 0; i < len(s); i++ {
        if s[i] != t[i] {
            mutations++
        }
    }
    
    return mutations
}

// ReadInput reads the input file and returns two DNA strings
func ReadInput(filename string) (string, string, error) {
    data, err := ioutil.ReadFile(filename)
    if err != nil {
        return "", "", err
    }
    
    lines := strings.Split(strings.TrimSpace(string(data)), "\n")
    if len(lines) < 2 {
        return "", "", fmt.Errorf("input file must contain at least 2 lines")
    }
    
    return lines[0], lines[1], nil
}

func main() {
    // Example usage with sample data
    s := "GAGCCTACTAACGGGAT"
    t := "CATCGGTATCGGTACCT"
    
    result := CountPointMutations(s, t)
    fmt.Printf("Hamming distance between %s and %s: %d\n", s, t, result)
    
    // For actual Rosalind input file
    // s, t, err := ReadInput("rosalind_hamm.txt")
    // if err != nil {
    //     fmt.Printf("Error reading input: %v\n", err)
    //     return
    // }
    // 
    // result := CountPointMutations(s, t)
    // fmt.Printf("Result: %d\n", result)
}
```

## Explanation

### Algorithm Steps:
1. **Input Validation**: Check that both strings have equal length
2. **Character-by-Character Comparison**: Iterate through each position in the strings
3. **Mutation Counting**: Increment counter when characters differ at any position
4. **Return Result**: Return the total count of differences

### Time and Space Complexity:
- **Time Complexity**: O(n) where n is the length of the strings
- **Space Complexity**: O(1) - only using a constant amount of extra space

### Example:
For strings:
- s = "GAGCCTACTAACGGGAT"
- t = "CATCGGTATCGGTACCT"

Position-by-position comparison:
```
G A G C C T A C T A A C G G G A T
C A T C G G T A T C G G T A C C T
^   ^   ^   ^   ^   ^   ^   ^   ^
D   D   D   D   D   D   D   D   D
```

There are 7 positions where characters differ, so the Hamming distance is 7.

## Sample Input/Output

**Input:**
```
GAGCCTACTAACGGGAT
CATCGGTATCGGTACCT
```

**Output:**
```
7
```

This solution efficiently computes the Hamming distance between two DNA sequences, which is a fundamental operation in bioinformatics for comparing genetic sequences.

