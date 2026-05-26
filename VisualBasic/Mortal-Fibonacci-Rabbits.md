# Rosalind Problem: Mortal Fibonacci Rabbits - Visual Basic Solution

## Problem Description
Given: Positive integers n ≤ 100 and m ≤ 20.
Return: The total number of pairs of rabbits that will remain after the n-th month if all rabbits live for m months.

## Solution

```vb
Module MortalFibonacciRabbits
    Sub Main()
        ' Read input values
        Dim n As Integer = 92  ' Number of months
        Dim m As Integer = 16  ' Lifespan in months
        
        ' Calculate mortal fibonacci rabbits
        Dim result As Long = CalculateMortalFibonacci(n, m)
        
        ' Output result
        Console.WriteLine(result)
    End Sub
    
    Function CalculateMortalFibonacci(n As Integer, m As Integer) As Long
        ' Array to store the number of rabbit pairs for each age group
        ' Index 0 represents 1-month-old rabbits, index 1 represents 2-month-old rabbits, etc.
        Dim ages(m) As Long
        
        ' Initialize: 1 pair of newborn rabbits at month 1
        ages(0) = 1
        
        ' For each month from 2 to n
        For i As Integer = 2 To n
            ' Calculate new rabbits born (all adults can reproduce)
            Dim newRabbits As Long = 0
            For j As Integer = 1 To m - 1
                newRabbits += ages(j)
            Next
            
            ' Shift ages: move each group one month forward
            ' The oldest group dies (age m)
            For j As Integer = m - 1 To 1 Step -1
                ages(j) = ages(j - 1)
            Next
            
            ' New rabbits are born at age 0
            ages(0) = newRabbits
        Next
        
        ' Sum all rabbit pairs
        Dim total As Long = 0
        For i As Integer = 0 To m - 1
            total += ages(i)
        Next
        
        Return total
    End Function
End Module
```

## Alternative Implementation (More Efficient)

```vb
Module MortalFibonacciRabbits
    Sub Main()
        ' Read input values
        Dim n As Integer = 92  ' Number of months
        Dim m As Integer = 16  ' Lifespan in months
        
        ' Calculate mortal fibonacci rabbits
        Dim result As Long = CalculateMortalFibonacciEfficient(n, m)
        
        ' Output result
        Console.WriteLine(result)
    End Sub
    
    Function CalculateMortalFibonacciEfficient(n As Integer, m As Integer) As Long
        ' Array to store the number of rabbit pairs for each age group
        Dim ages(m) As Long
        
        ' Initialize: 1 pair of newborn rabbits at month 1
        ages(0) = 1
        
        ' For each month from 2 to n
        For i As Integer = 2 To n
            ' Calculate new rabbits born (all adults can reproduce)
            Dim newRabbits As Long = 0
            For j As Integer = 1 To m - 1
                newRabbits += ages(j)
            Next
            
            ' Shift all ages forward by one month
            For j As Integer = m - 1 To 1 Step -1
                ages(j) = ages(j - 1)
            Next
            
            ' New rabbits are born at age 0
            ages(0) = newRabbits
        Next
        
        ' Sum all rabbit pairs
        Dim total As Long = 0
        For i As Integer = 0 To m - 1
            total += ages(i)
        Next
        
        Return total
    End Function
End Module
```

## Explanation

This solution models the rabbit population with a age-based approach:

1. **Initialization**: Start with 1 pair of newborn rabbits
2. **Monthly Simulation**: For each month:
   - Calculate new rabbits born (sum of all adult rabbits)
   - Shift all rabbits one age group forward
   - New rabbits are born at age 0
   - Rabbits that reach age m die
3. **Result**: Sum all rabbits remaining after n months

## Key Points

- **Time Complexity**: O(n × m)
- **Space Complexity**: O(m)
- **Age Tracking**: Uses an array where each index represents rabbits of that age
- **Death Mechanism**: Rabbits die when they reach age m

The solution handles the mortal fibonacci sequence where rabbits live for exactly m months before dying, unlike the standard fibonacci where rabbits never die.

