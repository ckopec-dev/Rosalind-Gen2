# Rosalind Problem: Counting Subsets

## Problem Statement
Given: A positive integer n ≤ 1000.
Return: The total number of subsets of {1,2,...,n} modulo 1,000,000.

## Solution

```csharp
using System;
using System.Numerics;

class Program
{
    static void Main()
    {
        // Read input
        int n = int.Parse(Console.ReadLine());
        
        // Calculate 2^n mod 1000000
        // Each element can either be included or excluded from a subset
        // So for n elements, there are 2^n possible subsets
        BigInteger result = BigInteger.ModPow(2, n, 1000000);
        
        Console.WriteLine(result);
    }
}
```

## Explanation

This problem is based on a fundamental combinatorial principle:

1. **Understanding the Problem**: 
   - We have a set with n elements: {1, 2, ..., n}
   - We need to count all possible subsets of this set
   - Each subset can be formed by either including or excluding each element

2. **Mathematical Insight**:
   - For each element in the set, we have 2 choices: include it or exclude it
   - With n elements, the total number of combinations is 2^n
   - This includes the empty set (when all elements are excluded) and the full set itself

3. **Implementation Details**:
   - Since n can be up to 1000, 2^n will be an extremely large number
   - We use `BigInteger.ModPow` to efficiently compute 2^n mod 1,000,000
   - This avoids overflow issues and computes the result modulo 1,000,000 as required

4. **Example**:
   - If n = 3, the set is {1, 2, 3}
   - Subsets: {}, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3}
   - Total: 2^3 = 8 subsets
   - Result: 8 mod 1000000 = 8

The time complexity is O(log n) due to the modular exponentiation, and space complexity is O(1).

