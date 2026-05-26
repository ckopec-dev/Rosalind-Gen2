# Rosalind Problem: Fibonacci Numbers (FIB)

## Problem Statement
Given: A positive integer n ≤ 25.

Return: The value of the Fibonacci sequence at position n.

## Solution in C#

```csharp
using System;

public class Fibonacci
{
    public static long CalculateFibonacci(int n)
    {
        if (n <= 0)
            throw new ArgumentException("n must be a positive integer");
        
        if (n == 1 || n == 2)
            return 1;
        
        long prev2 = 1;  // F(1)
        long prev1 = 1;  // F(2)
        long current = 0;
        
        for (int i = 3; i <= n; i++)
        {
            current = prev1 + prev2;
            prev2 = prev1;
            prev1 = current;
        }
        
        return current;
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        int n = 5;  // Example input
        long result = CalculateFibonacci(n);
        Console.WriteLine($"F({n}) = {result}");
        
        // For the actual Rosalind problem, you would read from input
        // string input = Console.ReadLine();
        // int n = int.Parse(input);
        // Console.WriteLine(CalculateFibonacci(n));
    }
}
```

## Alternative Recursive Solution (Less Efficient)

```csharp
using System;

public class FibonacciRecursive
{
    public static long CalculateFibonacciRecursive(int n)
    {
        if (n <= 0)
            throw new ArgumentException("n must be a positive integer");
        
        if (n == 1 || n == 2)
            return 1;
        
        return CalculateFibonacciRecursive(n - 1) + CalculateFibonacciRecursive(n - 2);
    }
    
    public static void Main(string[] args)
    {
        int n = 5;
        long result = CalculateFibonacciRecursive(n);
        Console.WriteLine($"F({n}) = {result}");
    }
}
```

## Explanation

### Approach 1: Iterative (Recommended)
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)
- Uses two variables to keep track of the previous two Fibonacci numbers
- More efficient for larger values of n

### Approach 2: Recursive
- **Time Complexity**: O(2^n)
- **Space Complexity**: O(n) due to call stack
- Simple but inefficient for large n
- Not suitable for the given constraint (n ≤ 25) but demonstrates the concept

### Key Points
1. The Fibonacci sequence starts with F(1) = 1, F(2) = 1
2. Each subsequent number is the sum of the two preceding numbers
3. For n = 5, the sequence is: 1, 1, 2, 3, 5
4. The answer for F(5) is 5

### Sample Input/Output
- Input: 5
- Output: 5

The iterative solution is preferred for its efficiency and scalability.

