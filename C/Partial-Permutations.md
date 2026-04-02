# Rosalind Problem: Partial Permutations

## Problem Statement
Partial permutations are an extension of permutations where we only care about the first k elements of a permutation. Given positive integers n and k, the task is to calculate the number of partial permutations P(n,k) = n!/(n-k)!.

## Solution in C

```c
#include <stdio.h>
#include <stdlib.h>

// Function to calculate partial permutations P(n,k)
long long partial_permutations(int n, int k) {
    if (k > n) return 0;
    if (k == 0) return 1;
    
    long long result = 1;
    const int MOD = 1000000; // Answer should be taken modulo 1,000,000
    
    // Calculate n * (n-1) * ... * (n-k+1)
    for (int i = 0; i < k; i++) {
        result = (result * (n - i)) % MOD;
    }
    
    return result;
}

int main() {
    int n, k;
    
    // Read input from stdin
    scanf("%d %d", &n, &k);
    
    // Calculate and print the result
    long long result = partial_permutations(n, k);
    printf("%lld\n", result);
    
    return 0;
}
```

## Explanation

1. **Problem Understanding**: 
   - Partial permutations P(n,k) = n!/(n-k)! = n × (n-1) × ... × (n-k+1)
   - This represents the number of ways to arrange k objects from n distinct objects

2. **Algorithm**:
   - Handle edge cases: if k > n, return 0; if k = 0, return 1
   - Calculate the product n × (n-1) × ... × (n-k+1) directly
   - Apply modulo 1,000,000 as required by the problem

3. **Key Features**:
   - Uses `long long` to handle potentially large numbers
   - Applies modular arithmetic to prevent overflow
   - Efficient O(k) time complexity
   - Handles all edge cases properly

## Sample Input/Output

**Input:**
```
21 7
```

**Output:**
```
51200
```

## Time and Space Complexity

- **Time Complexity**: O(k)
- **Space Complexity**: O(1)

The solution efficiently computes the partial permutation without calculating large factorials, making it suitable for the given constraints.

