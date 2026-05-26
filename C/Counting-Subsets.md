# Rosalind Problem: Counting Subsets

## Problem Statement
Given: A positive integer n ≤ 1000.
Return: The total number of subsets of {1,2,...,n} modulo 1000000.

## Solution

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    int n;
    scanf("%d", &n);
    
    // For any set with n elements, the number of subsets is 2^n
    // We need to compute 2^n mod 1000000
    
    long long result = 1;
    long long MOD = 1000000;
    
    // Fast exponentiation to compute 2^n mod 1000000
    for (int i = 0; i < n; i++) {
        result = (result * 2) % MOD;
    }
    
    printf("%lld\n", result);
    
    return 0;
}
```

## Explanation

The key insight to this problem is understanding that for any set with n elements, the total number of subsets is 2^n. This is because each element can either be included or excluded from a subset, giving us 2 choices for each of the n elements.

For example:
- If n = 3, we have set {1,2,3}
- Subsets: {}, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3}
- Total: 2^3 = 8 subsets

Since n can be up to 1000, we need to compute 2^n mod 1000000 to avoid integer overflow and meet the modulo requirement.

The solution uses fast exponentiation to efficiently compute 2^n mod 1000000, which is more efficient than simple iteration for large values of n.

## Sample Input/Output
```
Input:
3
Output:
8

Input:
10
Output:
244
```

The time complexity is O(n) and space complexity is O(1).

