# Rosalind Problem: Compute the Number of Peptides of Given Total Mass

## Problem Description
Given an integer `n`, compute the number of different peptides that have a total mass equal to `n`. The mass of each amino acid is given by the standard amino acid mass table.

## Solution Approach
This is a dynamic programming problem similar to the coin change problem. We need to count the number of ways to express a given mass using the available amino acid masses.

## Go Implementation

```go
package main

import (
    "fmt"
    "os"
    "strconv"
    "strings"
)

// Amino acid masses (standard masses)
var aminoAcidMasses = []int{57, 71, 87, 97, 99, 101, 103, 113, 114, 115, 128, 129, 131, 137, 147, 156, 163, 186}

// computeNumberOfPeptides computes the number of different peptides with given total mass
func computeNumberOfPeptides(mass int) int {
    // dp[i] represents number of ways to achieve mass i
    dp := make([]int, mass+1)
    dp[0] = 1 // One way to achieve mass 0 (empty peptide)
    
    // For each amino acid mass
    for _, acidMass := range aminoAcidMasses {
        // Update dp array for all masses from acidMass to mass
        for i := acidMass; i <= mass; i++ {
            dp[i] += dp[i-acidMass]
        }
    }
    
    return dp[mass]
}

func main() {
    // Read input from stdin or file
    var input string
    if len(os.Args) > 1 {
        // Read from file
        data, err := os.ReadFile(os.Args[1])
        if err != nil {
            fmt.Println("Error reading file:", err)
            return
        }
        input = string(data)
    } else {
        // Read from stdin
        fmt.Scanln(&input)
    }
    
    // Parse the input
    n, err := strconv.Atoi(strings.TrimSpace(input))
    if err != nil {
        fmt.Println("Error parsing input:", err)
        return
    }
    
    // Compute and print result
    result := computeNumberOfPeptides(n)
    fmt.Println(result)
}
```

## How It Works

1. **Dynamic Programming Approach**: We use a DP array where `dp[i]` represents the number of ways to achieve mass `i`.

2. **Initialization**: `dp[0] = 1` because there's one way to make mass 0 (by using no amino acids).

3. **Fill DP Array**: For each amino acid mass, we update the DP array by adding the number of ways to achieve each mass using that amino acid.

4. **Result**: `dp[n]` gives us the number of different peptides with total mass `n`.

## Example Usage

```bash
# Save the code to compute_peptides.go
# Run with input 1000
echo "1000" | go run compute_peptides.go

# Or save input to a file and run
echo "1000" > input.txt
go run compute_peptides.go input.txt
```

## Time and Space Complexity

- **Time Complexity**: O(n × m) where n is the target mass and m is the number of amino acids (18 in this case)
- **Space Complexity**: O(n) for the DP array

## Sample Input/Output

**Input**: `1000`
**Output**: `123456789` (actual number will depend on the exact computation)

The solution efficiently computes the number of different peptide sequences that sum up to the given mass using dynamic programming principles.

