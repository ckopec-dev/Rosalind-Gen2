# Rosalind Problem: Error Correction in Reads (Go Solution)

## Problem Understanding

The problem requires us to identify and correct sequencing errors in DNA reads. The approach involves:
1. Finding reads that appear only once (potential errors)
2. Finding reads that appear multiple times (correct reads)
3. For each single-occurrence read, checking if it's a one-mismatch version of a correct read

## Solution Approach

```go
package main

import (
    "fmt"
    "os"
    "bufio"
    "strings"
)

// reverseComplement returns the reverse complement of a DNA string
func reverseComplement(s string) string {
    complement := make([]byte, len(s))
    for i, c := range s {
        switch c {
        case 'A':
            complement[len(s)-1-i] = 'T'
        case 'T':
            complement[len(s)-1-i] = 'A'
        case 'G':
            complement[len(s)-1-i] = 'C'
        case 'C':
            complement[len(s)-1-i] = 'G'
        }
    }
    return string(complement)
}

// hammingDistance calculates the Hamming distance between two strings
func hammingDistance(s1, s2 string) int {
    if len(s1) != len(s2) {
        return -1 // Invalid: different lengths
    }
    distance := 0
    for i := 0; i < len(s1); i++ {
        if s1[i] != s2[i] {
            distance++
        }
    }
    return distance
}

// findErrorCorrections identifies error corrections in DNA reads
func findErrorCorrections(reads []string) []string {
    // Count occurrences of each read
    count := make(map[string]int)
    for _, read := range reads {
        count[read]++
    }
    
    // Separate correct reads (appearing multiple times) and erroneous reads (appearing once)
    var correctReads []string
    var erroneousReads []string
    
    for read, freq := range count {
        if freq > 1 {
            correctReads = append(correctReads, read)
        } else {
            erroneousReads = append(erroneousReads, read)
        }
    }
    
    // For each erroneous read, find its correction
    var corrections []string
    for _, errRead := range erroneousReads {
        // Check if errRead is a one-mismatch version of any correct read
        for _, corrRead := range correctReads {
            if hammingDistance(errRead, corrRead) == 1 {
                corrections = append(corrections, fmt.Sprintf("%s->%s", errRead, corrRead))
                break
            }
            // Also check reverse complement
            rc := reverseComplement(errRead)
            if hammingDistance(rc, corrRead) == 1 {
                corrections = append(corrections, fmt.Sprintf("%s->%s", errRead, corrRead))
                break
            }
        }
    }
    
    return corrections
}

func main() {
    // Read input from stdin or file
    var scanner *bufio.Scanner
    if len(os.Args) > 1 {
        file, err := os.Open(os.Args[1])
        if err != nil {
            panic(err)
        }
        defer file.Close()
        scanner = bufio.NewScanner(file)
    } else {
        scanner = bufio.NewScanner(os.Stdin)
    }
    
    var reads []string
    var currentRead string
    
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if strings.HasPrefix(line, ">") {
            if currentRead != "" {
                reads = append(reads, currentRead)
            }
            currentRead = ""
        } else if line != "" {
            currentRead += line
        }
    }
    
    if currentRead != "" {
        reads = append(reads, currentRead)
    }
    
    // For Rosalind format, reads are given directly without FASTA headers
    // Let's read all lines as separate reads
    if len(reads) == 0 {
        var allReads []string
        scanner = bufio.NewScanner(os.Stdin)
        for scanner.Scan() {
            line := strings.TrimSpace(scanner.Text())
            if line != "" {
                allReads = append(allReads, line)
            }
        }
        reads = allReads
    }
    
    // Process the reads
    corrections := findErrorCorrections(reads)
    
    // Print results
    for _, correction := range corrections {
        fmt.Println(correction)
    }
}
```

## Alternative Cleaner Version

```go
package main

import (
    "bufio"
    "fmt"
    "os"
    "strings"
)

func reverseComplement(s string) string {
    result := make([]byte, len(s))
    for i, c := range s {
        switch c {
        case 'A': result[len(s)-1-i] = 'T'
        case 'T': result[len(s)-1-i] = 'A'
        case 'G': result[len(s)-1-i] = 'C'
        case 'C': result[len(s)-1-i] = 'G'
        }
    }
    return string(result)
}

func hammingDistance(s1, s2 string) int {
    if len(s1) != len(s2) {
        return -1
    }
    distance := 0
    for i := 0; i < len(s1); i++ {
        if s1[i] != s2[i] {
            distance++
        }
    }
    return distance
}

func findErrorCorrections(reads []string) []string {
    // Count read frequencies
    count := make(map[string]int)
    for _, read := range reads {
        count[read]++
    }
    
    // Separate correct and erroneous reads
    var correct []string
    var erroneous []string
    
    for read, freq := range count {
        if freq > 1 {
            correct = append(correct, read)
        } else {
            erroneous = append(erroneous, read)
        }
    }
    
    // Find corrections
    var corrections []string
    for _, err := range erroneous {
        for _, corr := range correct {
            if hammingDistance(err, corr) == 1 {
                corrections = append(corrections, fmt.Sprintf("%s->%s", err, corr))
                break
            }
            rc := reverseComplement(err)
            if hammingDistance(rc, corr) == 1 {
                corrections = append(corrections, fmt.Sprintf("%s->%s", err, corr))
                break
            }
        }
    }
    
    return corrections
}

func main() {
    scanner := bufio.NewScanner(os.Stdin)
    var reads []string
    
    for scanner.Scan() {
        line := strings.TrimSpace(scanner.Text())
        if line != "" {
            reads = append(reads, line)
        }
    }
    
    corrections := findErrorCorrections(reads)
    for _, corr := range corrections {
        fmt.Println(corr)
    }
}
```

## How to Use

1. Save the code as `error_correction.go`
2. Compile: `go build error_correction.go`
3. Run with input file: `./error_correction < input.txt`
4. Or pipe input: `cat input.txt | ./error_correction`

## Example Input
```
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAATTT
AAAT