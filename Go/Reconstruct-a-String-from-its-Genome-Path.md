# Rosalind Problem: Reconstruct a String from its Genome Path

## Problem Description
Given a sequence of k-mers that form a genome path, reconstruct the original DNA string. The genome path consists of a sequence of k-mers where each k-mer overlaps with the next one by k-1 characters.

## Solution Approach
1. Take the first k-mer and use it as the starting string
2. For each subsequent k-mer, add only the last character (since the first k-1 characters overlap)
3. Concatenate all the unique characters to form the complete string

## Go Implementation

```go
package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

// ReconstructStringFromGenomePath takes a slice of k-mers and returns the reconstructed string
func ReconstructStringFromGenomePath(genomePath []string) string {
    if len(genomePath) == 0 {
        return ""
    }
    
    // Start with the first k-mer
    result := genomePath[0]
    
    // For each subsequent k-mer, add only the last character
    // since the first k-1 characters overlap with the previous k-mer
    for i := 1; i < len(genomePath); i++ {
        result += string(genomePath[i][len(genomePath[i])-1])
    }
    
    return result
}

func main() {
    // Read input from stdin
    scanner := bufio.NewScanner(os.Stdin)
    var genomePath []string
    
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line != "" {
            genomePath = append(genomePath, line)
        }
    }
    
    if err := scanner.Err(); err != nil {
        fmt.Fprintf(os.Stderr, "Error reading input: %v\n", err)
        os.Exit(1)
    }
    
    // Reconstruct the string
    result := ReconstructStringFromGenomePath(genomePath)
    fmt.Println(result)
}
```

## Example

**Input:**
```
ACCGA
CCGAA
CGAAG
GAAGC
AAGCT
```

**Process:**
1. Start with first k-mer: "ACCGA"
2. Next k-mer "CCGAA": add last character 'A' → "ACCGAA"
3. Next k-mer "CGAAG": add last character 'G' → "ACCGAAG"
4. Next k-mer "GAAGC": add last character 'C' → "ACCGAAGC"
5. Next k-mer "AAGCT": add last character 'T' → "ACCGAAGCT"

**Output:**
```
ACCGAAGCT
```

## How it works

The key insight is that in a genome path, each k-mer overlaps with the next one by exactly k-1 characters. So when reconstructing:
- We start with the first k-mer completely
- For each subsequent k-mer, we only need to add the last character (the first k-1 characters are already covered by the overlap)
- This gives us the complete original string

## Time and Space Complexity
- **Time Complexity**: O(n × k) where n is the number of k-mers and k is the k-mer length
- **Space Complexity**: O(n × k) for storing the result string

## Usage
Save the code to a file (e.g., `genome_path.go`), compile it with `go build genome_path.go`, and run it with input from stdin or redirect input from a file.

