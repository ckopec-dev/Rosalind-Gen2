# Rosalind Problem: Counting Unrooted Binary Trees

## Problem Description
Given a collection of taxa, we want to count the number of distinct unrooted binary trees that can be formed with these taxa as leaves.

For n taxa, the number of distinct unrooted binary trees is (2n-5)!! (double factorial), where:
- For n = 1, answer is 0 (no tree possible with 1 leaf)
- For n = 2, answer is 0 (no tree possible with 2 leaves)
- For n ≥ 3, answer is (2n-5)!! = (2n-5) × (2n-7) × ... × 3 × 1

## Visual Basic Solution

```vb
Module CountingUnrootedBinaryTrees
    ' Function to calculate double factorial
    Function DoubleFactorial(n As Long) As Long
        If n <= 0 Then
            Return 1
        End If
        
        Dim result As Long = 1
        Dim i As Long = n
        
        While i > 0
            result = result * i
            i = i - 2
        End While
        
        Return result
    End Function
    
    ' Function to count unrooted binary trees
    Function CountUnrootedBinaryTrees(n As Integer) As Long
        If n < 3 Then
            Return 0
        End If
        
        ' For n taxa, the number of unrooted binary trees is (2n-5)!!
        Dim k As Long = 2 * n - 5
        Return DoubleFactorial(k)
    End Function
    
    Sub Main()
        ' Read input from console
        Console.Write("Enter the number of taxa: ")
        Dim n As Integer = Integer.Parse(Console.ReadLine())
        
        ' Calculate and display result
        Dim result As Long = CountUnrootedBinaryTrees(n)
        Console.WriteLine("Number of unrooted binary trees: " & result)
        
        ' Example test cases
        Console.WriteLine("Test cases:")
        Console.WriteLine("n=3: " & CountUnrootedBinaryTrees(3))  ' Expected: 1
        Console.WriteLine("n=4: " & CountUnrootedBinaryTrees(4))  ' Expected: 3
        Console.WriteLine("n=5: " & CountUnrootedBinaryTrees(5))  ' Expected: 15
        Console.WriteLine("n=6: " & CountUnrootedBinaryTrees(6))  ' Expected: 105
        
        Console.ReadLine()
    End Sub
End Module
```

## Explanation

1. **DoubleFactorial Function**: Calculates the double factorial of a number (n!! = n × (n-2) × (n-4) × ...)

2. **CountUnrootedBinaryTrees Function**: 
   - Returns 0 for n < 3 (no valid unrooted binary trees possible)
   - For n ≥ 3, calculates (2n-5)!! which gives the number of distinct unrooted binary trees

3. **Formula**: The number of distinct unrooted binary trees with n leaves is (2n-5)!! where:
   - n = number of taxa/leaves
   - (2n-5)!! = (2n-5) × (2n-7) × ... × 3 × 1

## Example Output
```
Enter the number of taxa: 5
Number of unrooted binary trees: 15
Test cases:
n=3: 1
n=4: 3
n=5: 15
n=6: 105
```

## Time Complexity
- O(n) where n is the number of taxa
- The double factorial calculation requires approximately n/2 operations

## Space Complexity
- O(1) - only using a constant amount of extra space

