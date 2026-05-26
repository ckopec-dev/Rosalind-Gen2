# Rosalind Problem: Mortal Fibonacci Rabbits

## Problem Description
In this problem, we need to calculate the number of rabbit pairs after `n` months, where each pair of rabbits lives for `m` months. The rabbits mature after 1 month and produce a new pair every month thereafter.

## Solution in C#

```csharp
using System;
using System.Linq;

public class Program
{
    public static void Main()
    {
        // Example input: n = 6, m = 3
        int n = 6;  // number of months
        int m = 3;  // lifespan in months
        
        long result = MortalFibonacciRabbits(n, m);
        Console.WriteLine(result);
    }
    
    public static long MortalFibonacciRabbits(int n, int m)
    {
        // Create an array to store the number of rabbit pairs for each age group
        // Index 0 represents 1-month-old rabbits, index 1 represents 2-month-old rabbits, etc.
        // We need m+1 slots because rabbits live for m months
        long[] rabbits = new long[m + 1];
        
        // Initially, we have 1 pair of 1-month-old rabbits
        rabbits[0] = 1;
        
        // For each month
        for (int month = 2; month <= n; month++)
        {
            // New rabbits are born from rabbits that are at least 2 months old
            long newRabbits = rabbits.Skip(1).Sum();
            
            // Shift all age groups forward by one month
            // The oldest rabbits die (so we remove the last element)
            for (int i = m; i >= 1; i--)
            {
                rabbits[i] = rabbits[i - 1];
            }
            
            // New rabbits are born
            rabbits[0] = newRabbits;
        }
        
        // Sum all rabbit pairs
        return rabbits.Sum();
    }
}
```

## Alternative Implementation (More Explicit)

```csharp
using System;
using System.Numerics;

public class MortalFibonacciRabbitsSolver
{
    public static BigInteger Solve(int n, int m)
    {
        // For the first m months, we simply have the standard Fibonacci sequence
        // But after m months, rabbits start dying
        
        // Array to store rabbit counts for each age group
        // rabbits[i] represents number of rabbits that are (i+1) months old
        BigInteger[] rabbits = new BigInteger[m];
        
        // Initially, we have 1 pair of 1-month-old rabbits
        rabbits[0] = 1;
        
        // For each month from 2 to n
        for (int month = 2; month <= n; month++)
        {
            // Calculate new rabbits (sum of all rabbits that can reproduce)
            BigInteger newRabbits = 0;
            for (int i = 1; i < m; i++)
            {
                newRabbits += rabbits[i];
            }
            
            // Shift all age groups
            for (int i = m - 1; i > 0; i--)
            {
                rabbits[i] = rabbits[i - 1];
            }
            
            // New rabbits are born (at age 1)
            rabbits[0] = newRabbits;
        }
        
        // Return total rabbit pairs
        return rabbits.Sum();
    }
    
    public static void Main()
    {
        // Example: n = 6, m = 3
        int n = 6;
        int m = 3;
        
        BigInteger result = Solve(n, m);
        Console.WriteLine($"After {n} months with {m}-month lifespan: {result}");
    }
}
```

## Explanation

The solution works by tracking rabbit populations by age groups:

1. **Initialization**: Start with 1 pair of 1-month-old rabbits
2. **Monthly Simulation**: 
   - Calculate new rabbits born (sum of all rabbits that are at least 2 months old)
   - Shift all existing rabbits to the next age group
   - Add new rabbits to the youngest age group
3. **Death Handling**: Rabbits die after `m` months, so we don't count rabbits in the oldest age group

## Time and Space Complexity
- **Time Complexity**: O(n × m) where n is the number of months and m is the lifespan
- **Space Complexity**: O(m) for storing the age groups

## Example Run
For n = 6, m = 3:
- Month 1: 1 pair (1-month-old)
- Month 2: 1 pair (2-month-old) 
- Month 3: 2 pairs (1-month-old + 1-month-old)
- Month 4: 2 pairs (1-month-old + 1-month-old) + 1 pair (2-month-old) = 3 pairs
- Month 5: 3 pairs (1-month-old + 1-month-old + 1-month-old) + 1 pair (2-month-old) = 4 pairs
- Month 6: 4 pairs (1-month-old + 1-month-old + 1-month-old + 1-month-old) + 1 pair (2-month-old) + 1 pair (3-month-old) = 6 pairs

The answer is **6**.

