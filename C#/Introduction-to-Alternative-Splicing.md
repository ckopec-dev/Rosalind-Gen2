# Rosalind Problem: Introduction to Alternative Splicing

## Problem Description
In this problem, we need to calculate the number of ways to choose k items from n items, where k can vary from 0 to n. This is essentially computing the sum of binomial coefficients C(n,k) for k from 0 to n.

## Solution Approach
The key insight is that the sum of all binomial coefficients C(n,k) for k from 0 to n equals 2^n. This is because:
- C(n,0) + C(n,1) + C(n,2) + ... + C(n,n) = 2^n
- This follows from the binomial theorem: (1+1)^n = 2^n

## C# Implementation

```csharp
using System;
using System.Numerics;

class Program
{
    static void Main()
    {
        // Read input values
        string[] input = Console.ReadLine().Split();
        int n = int.Parse(input[0]);
        int k = int.Parse(input[1]);
        
        // Calculate the sum of binomial coefficients C(n,0) + C(n,1) + ... + C(n,k)
        // But since we want all possible combinations (k from 0 to n), 
        // the answer is 2^n
        BigInteger result = BigInteger.Pow(2, n);
        
        // Since we're working with modulo arithmetic (as per typical Rosalind problems)
        // and the result can be very large, we'll return the result modulo 1000000
        Console.WriteLine(result % 1000000);
    }
}
```

## Alternative Implementation (if we need to compute C(n,k) for specific k values)

```csharp
using System;
using System.Numerics;

class Program
{
    static void Main()
    {
        // Read input values
        string[] input = Console.ReadLine().Split();
        int n = int.Parse(input[0]);
        int k = int.Parse(input[1]);
        
        // Calculate sum of binomial coefficients from C(n,0) to C(n,k)
        BigInteger sum = 0;
        for (int i = 0; i <= k; i++)
        {
            sum += BinomialCoefficient(n, i);
        }
        
        // Return result modulo 1000000
        Console.WriteLine(sum % 1000000);
    }
    
    static BigInteger BinomialCoefficient(int n, int k)
    {
        if (k > n - k)
            k = n - k;
            
        BigInteger result = 1;
        for (int i = 0; i < k; i++)
        {
            result = result * (n - i) / (i + 1);
        }
        
        return result;
    }
}
```

## Explanation

The first approach is more efficient and correct for the problem as stated. Since we're dealing with alternative splicing, we want to count all possible ways to include or exclude each of the n exons, which gives us 2^n total possibilities.

The second approach shows how to compute individual binomial coefficients if needed, but for this problem, we're essentially looking for the total number of subsets of n elements, which is 2^n.

## Sample Input/Output

**Input:**
```
6 3
```

**Output:**
```
64
```

This represents 2^6 = 64 possible combinations of exons when we have 6 exons total.

## Time Complexity
- First approach: O(1) - just computing 2^n
- Second approach: O(k) - computing binomial coefficients up to k

## Space Complexity
O(1) for both approaches

The first approach is preferred for this specific problem since it directly computes the mathematical result using the property that the sum of all binomial coefficients equals 2^n.

