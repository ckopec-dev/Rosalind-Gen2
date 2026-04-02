# Rosalind Problem: Insertion Sort - Visual Basic Solution

## Problem Understanding

The insertion sort problem requires us to sort an array of integers using the insertion sort algorithm and return the number of swaps performed during the sorting process.

## Solution

```vb
Module InsertionSort
    Sub Main()
        ' Read input from console
        Dim input As String = Console.ReadLine()
        Dim n As Integer = Integer.Parse(input)
        
        Dim arr() As Integer = Console.ReadLine().Split(" ").Select(Function(x) Integer.Parse(x)).ToArray()
        
        ' Perform insertion sort and count swaps
        Dim swaps As Integer = InsertionSortArray(arr)
        
        ' Output the result
        Console.WriteLine(swaps)
    End Sub
    
    Function InsertionSortArray(arr() As Integer) As Integer
        Dim swaps As Integer = 0
        Dim n As Integer = arr.Length
        
        ' Iterate through the array starting from the second element
        For i As Integer = 1 To n - 1
            Dim key As Integer = arr(i)
            Dim j As Integer = i - 1
            
            ' Move elements that are greater than key one position ahead
            While j >= 0 AndAlso arr(j) > key
                arr(j + 1) = arr(j)
                j -= 1
                swaps += 1
            End While
            
            ' Place key at its correct position
            arr(j + 1) = key
        Next
        
        Return swaps
    End Function
End Module
```

## Explanation

1. **Input Reading**: 
   - First line contains the number of elements `n`
   - Second line contains the array elements separated by spaces

2. **Insertion Sort Algorithm**:
   - Start from the second element (index 1) since a single element is already "sorted"
   - For each element, compare it with elements to its left
   - If an element to the left is greater, shift it one position to the right
   - Continue until we find the correct position for the current element
   - Count each shift operation as one swap

3. **Key Steps**:
   - Initialize `swaps` counter to 0
   - For each position `i` from 1 to n-1:
     - Store current element in `key`
     - Compare with elements to the left
     - Shift larger elements one position right
     - Increment swap counter for each shift
     - Place `key` in its correct position

4. **Output**: Return the total number of swaps performed

## Example

Input:
```
6
6 10 4 5 1 2
```

Output:
```
10
```

The algorithm correctly counts all the shifts needed to sort the array using the insertion sort method.

