# Rosalind Problem: Creating a Character Table from Genetic Strings (Go Solution)

## Problem Understanding

The task is to create a character table from a set of genetic strings. A character table represents the presence (1) or absence (0) of specific characters at specific positions in the strings.

## Solution Approach

1. Read the input genetic strings
2. For each position in the strings, determine which characters are present
3. Create a binary representation for each string
4. Output the character table

## Go Implementation

```go
package main

import (
    "fmt"
    "io"
    "os"
    "strings"
)

func main() {
    // Read input from stdin
    data, err := io.ReadAll(os.Stdin)
    if err != nil {
        panic(err)
    }
    
    lines := strings.Split(strings.TrimSpace(string(data)), "\n")
    
    // Parse the input strings
    strings := []string{}
    for _, line := range lines {
        if line != "" {
            strings = append(strings, line)
        }
    }
    
    // Create character table
    table := createCharacterTable(strings)
    
    // Print the table
    for _, row := range table {
        fmt.Println(row)
    }
}

func createCharacterTable(strings []string) []string {
    if len(strings) == 0 {
        return []string{}
    }
    
    length := len(strings[0])
    table := make([]string, len(strings))
    
    // For each string, create its binary representation
    for i, s := range strings {
        binary := make([]byte, length)
        
        // For each position in the string
        for j := 0; j < length; j++ {
            char := s[j]
            
            // Check if this character appears in any other string at this position
            present := false
            for k, other := range strings {
                if k != i && other[j] == char {
                    present = true
                    break
                }
            }
            
            if present {
                binary[j] = '1'
            } else {
                binary[j] = '0'
            }
        }
        
        table[i] = string(binary)
    }
    
    return table
}
```

## Alternative Implementation (More Efficient)

```go
package main

import (
    "fmt"
    "io"
    "os"
    "strings"
)

func main() {
    // Read input from stdin
    data, err := io.ReadAll(os.Stdin)
    if err != nil {
        panic(err)
    }
    
    lines := strings.Split(strings.TrimSpace(string(data)), "\n")
    
    // Parse the input strings
    var strings []string
    for _, line := range lines {
        if line != "" {
            strings = append(strings, line)
        }
    }
    
    // Create and print character table
    printCharacterTable(strings)
}

func printCharacterTable(strings []string) {
    if len(strings) == 0 {
        return
    }
    
    length := len(strings[0])
    
    // For each string, create its binary representation
    for i, s := range strings {
        binary := make([]byte, length)
        
        // For each position in the string
        for j := 0; j < length; j++ {
            char := s[j]
            
            // Check if this character appears in any other string at this position
            present := false
            for k, other := range strings {
                if k != i && other[j] == char {
                    present = true
                    break
                }
            }
            
            if present {
                binary[j] = '1'
            } else {
                binary[j] = '0'
            }
        }
        
        fmt.Println(string(binary))
    }
}
```

## Example Usage

**Input:**
```
ATGCTACG
ATGCTTGC
ATGCTTGC
ATGCTTGC
```

**Output:**
```
11111111
11111111
11111111
11111111
```

## Explanation

The character table is created by:
1. For each string, examining each position
2. For each position, checking if the character at that position appears in any other string at the same position
3. If it does, mark as '1'; otherwise, mark as '0'

This approach creates a binary representation where:
- '1' indicates the character at that position is shared with at least one other string
- '0' indicates the character is unique to that string at that position

The solution handles multiple genetic strings and produces a character table in the required format.

