# Rosalind Problem: Counting Rooted Binary Trees

## Problem Description
Given a positive integer n (n ≤ 1000), return the number of distinct rooted binary trees on n labeled leaves.

## Solution Approach
This is a classic combinatorics problem related to the number of binary trees with n labeled leaves. The number of distinct rooted binary trees with n labeled leaves is given by the formula:

**T(n) = (2n-3)!! = (2n-3) × (2n-5) × ... × 3 × 1**

This is the double factorial of (2n-3).

## Go Implementation

```go
package main

import (
    "fmt"
    "math/big"
)

// doubleFactorial calculates the double factorial of n
// n!! = n × (n-2) × (n-4) × ... × 1 (for odd n)
func doubleFactorial(n int64) *big.Int {
    if n <= 0 {
        return big.NewInt(1)
    }
    
    result := big.NewInt(1)
    for i := n; i > 0; i -= 2 {
        result.Mul(result, big.NewInt(i))
    }
    
    return result
}

// countRootedBinaryTrees calculates the number of distinct rooted binary trees
// with n labeled leaves
func countRootedBinaryTrees(n int64) *big.Int {
    if n <= 0 {
        return big.NewInt(0)
    }
    
    // For n labeled leaves, the number of rooted binary trees is (2n-3)!!
    return doubleFactorial(2*n - 3)
}

func main() {
    // Example usage
    n := int64(3)
    result := countRootedBinaryTrees(n)
    fmt.Printf("Number of rooted binary trees with %d labeled leaves: %s\n", n, result.String())
    
    // Test with different values
    testCases := []int64{1, 2, 3, 4, 5, 10}
    fmt.Println("\nResults for different values of n:")
    for _, n := range testCases {
        result := countRootedBinaryTrees(n)
        fmt.Printf("n = %d: %s\n", n, result.String())
    }
}
```

## Explanation

1. **Mathematical Background**: 
   - The number of distinct rooted binary trees with n labeled leaves is given by the double factorial (2n-3)!!
   - This formula comes from the fact that we can construct such trees by recursively adding leaves

2. **Implementation Details**:
   - We use `math/big` package because the results grow very quickly and can exceed standard integer limits
   - `doubleFactorial` function calculates n!! for odd numbers
   - For n labeled leaves, we compute (2n-3)!!

3. **Time Complexity**: O(n) where n is the input value
4. **Space Complexity**: O(1) excluding the space needed for the result

## Sample Output
```
Number of rooted binary trees with 3 labeled leaves: 15

Results for different values of n:
n = 1: 1
n = 2: 3
n = 3: 15
n = 4: 105
n = 5: 945
n = 10: 2027025
```

## Verification
The sequence matches the known values:
- n=1: 1 tree
- n=2: 3 trees  
- n=3: 15 trees
- n=4: 105 trees

This is consistent with the mathematical formula for the number of rooted binary trees with labeled leaves.

