# Rosalind Problem: Enumerating Unrooted Binary Trees

## Problem Statement
Given a collection of n taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is given by the formula: (2n-5)!! = (2n-5) × (2n-7) × (2n-9) × ... × 3 × 1

## Solution in Visual Basic

```vb
Module EnumeratingUnrootedBinaryTrees
    Sub Main()
        ' Read input from console
        Console.Write("Enter the number of taxa (n): ")
        Dim n As Integer = Integer.Parse(Console.ReadLine())
        
        ' Calculate the number of unrooted binary trees
        Dim result As Long = CountUnrootedBinaryTrees(n)
        
        ' Output the result
        Console.WriteLine($"Number of unrooted binary trees with {n} taxa: {result}")
        
        Console.ReadLine()
    End Sub
    
    ' Function to calculate the number of unrooted binary trees
    Function CountUnrootedBinaryTrees(n As Integer) As Long
        ' For n <= 1, there are no unrooted binary trees
        If n <= 1 Then
            Return 0
        End If
        
        ' For n = 2, there is 1 unrooted binary tree
        If n = 2 Then
            Return 1
        End If
        
        ' For n >= 3, use the formula (2n-5)!!
        ' This is the double factorial of (2n-5)
        Dim result As Long = 1
        Dim k As Integer = 2 * n - 5
        
        ' Calculate the double factorial
        While k >= 1
            result = result * k
            k = k - 2
        End While
        
        Return result
    End Function
End Module
```

## Explanation

The solution implements the mathematical formula for counting unrooted binary trees with n taxa:

1. **Formula**: The number of distinct unrooted binary trees with n taxa is (2n-5)!!, where !! denotes the double factorial.

2. **Double Factorial**: For an odd number (2n-5), the double factorial is the product of all odd numbers from that number down to 1.

3. **Special Cases**:
   - If n ≤ 1: No unrooted binary trees exist
   - If n = 2: There is exactly 1 unrooted binary tree

4. **Implementation Details**:
   - The program reads the number of taxa from user input
   - Uses a loop to calculate the double factorial
   - Returns the final count

## Example Usage

Input: n = 4
- Formula: (2×4-5)!! = 3!! = 3 × 1 = 3
- Output: "Number of unrooted binary trees with 4 taxa: 3"

Input: n = 5
- Formula: (2×5-5)!! = 5!! = 5 × 3 × 1 = 15
- Output: "Number of unrooted binary trees with 5 taxa: 15"

## Time Complexity
O(n) - The algorithm performs a loop that runs approximately n/2 times.

## Space Complexity
O(1) - Uses only a constant amount of extra space.

