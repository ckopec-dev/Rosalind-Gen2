# Rosalind Problem: Introduction to Alternative Splicing

## Problem Statement
In this problem, we need to calculate the number of ways to choose k items from n items, where k can vary from 0 to n. This is essentially calculating the sum of binomial coefficients C(n,k) for k = 0 to n, which equals 2^n.

## Solution Approach
The key insight is that the sum of all binomial coefficients C(n,k) for k = 0 to n is 2^n. This is because:
- C(n,0) + C(n,1) + C(n,2) + ... + C(n,n) = 2^n
- This represents all possible subsets of a set with n elements

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Function to calculate 2^n efficiently
long long power_of_two(int n) {
    long long result = 1;
    for (int i = 0; i < n; i++) {
        result *= 2;
    }
    return result;
}

int main() {
    int n, k;
    
    // Read input values
    printf("Enter n and k (space separated): ");
    scanf("%d %d", &n, &k);
    
    // Since we want all possible combinations from 0 to k,
    // we need to calculate 2^n (sum of all C(n,k) for k=0 to n)
    long long result = power_of_two(n);
    
    // Apply modulo 1000000 as required
    result = result % 1000000;
    
    printf("Result: %lld\n", result);
    
    return 0;
}
```

## Alternative Implementation (More Efficient)

```c
#include <stdio.h>

int main() {
    int n;
    
    // Read n
    printf("Enter n: ");
    scanf("%d", &n);
    
    // Calculate 2^n mod 1000000
    long long result = 1;
    const int MOD = 1000000;
    
    for (int i = 0; i < n; i++) {
        result = (result * 2) % MOD;
    }
    
    printf("Result: %lld\n", result);
    
    return 0;
}
```

## Sample Input/Output

**Input:**
```
6 4
```

**Output:**
```
Result: 64
```

## Explanation

For n = 6, we calculate 2^6 = 64. This represents:
- C(6,0) + C(6,1) + C(6,2) + C(6,3) + C(6,4) + C(6,5) + C(6,6) = 1 + 6 + 15 + 20 + 15 + 6 + 1 = 64

The problem is essentially asking for the total number of ways to choose subsets from a set of n elements, which is always 2^n.

## Time and Space Complexity
- **Time Complexity:** O(n) - we need to compute 2^n
- **Space Complexity:** O(1) - only using constant extra space

## Note
The problem might have different interpretations. If k is a specific value, we would need to calculate C(n,k) for that specific k. However, based on the problem description, it's asking for the sum of all possible combinations, which is 2^n.

