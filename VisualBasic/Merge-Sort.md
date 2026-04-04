# Rosalind Problem: Merge_Sort in Visual Basic

## Problem Description
Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: A sorted array A[1..n] in ascending order.

## Solution Approach
I'll implement merge sort algorithm in Visual Basic to sort the array efficiently with O(n log n) time complexity.

```vb
Module MergeSort
    Sub Main()
        ' Read input from console
        Dim n As Integer = Integer.Parse(Console.ReadLine())
        Dim input As String() = Console.ReadLine().Split(" ")
        
        ' Convert string array to integer array
        Dim arr(n - 1) As Integer
        For i As Integer = 0 To n - 1
            arr(i) = Integer.Parse(input(i))
        Next
        
        ' Perform merge sort
        MergeSortArray(arr, 0, n - 1)
        
        ' Output result
        For i As Integer = 0 To n - 1
            Console.Write(arr(i) & " ")
        Next
        Console.WriteLine()
    End Sub
    
    Sub MergeSortArray(arr() As Integer, left As Integer, right As Integer)
        If left < right Then
            Dim mid As Integer = left + (right - left) \ 2
            
            ' Recursively sort first and second halves
            MergeSortArray(arr, left, mid)
            MergeSortArray(arr, mid + 1, right)
            
            ' Merge the sorted halves
            Merge(arr, left, mid, right)
        End If
    End Sub
    
    Sub Merge(arr() As Integer, left As Integer, mid As Integer, right As Integer)
        ' Calculate sizes of subarrays
        Dim leftSize As Integer = mid - left + 1
        Dim rightSize As Integer = right - mid
        
        ' Create temporary arrays
        Dim leftArr(leftSize - 1) As Integer
        Dim rightArr(rightSize - 1) As Integer
        
        ' Copy data to temporary arrays
        For i As Integer = 0 To leftSize - 1
            leftArr(i) = arr(left + i)
        Next
        
        For j As Integer = 0 To rightSize - 1
            rightArr(j) = arr(mid + 1 + j)
        Next
        
        ' Merge the temporary arrays back
        Dim i As Integer = 0
        Dim j As Integer = 0
        Dim k As Integer = left
        
        While i < leftSize AndAlso j < rightSize
            If leftArr(i) <= rightArr(j) Then
                arr(k) = leftArr(i)
                i += 1
            Else
                arr(k) = rightArr(j)
                j += 1
            End If
            k += 1
        End While
        
        ' Copy remaining elements of leftArr[] if any
        While i < leftSize
            arr(k) = leftArr(i)
            i += 1
            k += 1
        End While
        
        ' Copy remaining elements of rightArr[] if any
        While j < rightSize
            arr(k) = rightArr(j)
            j += 1
            k += 1
        End While
    End Sub
End Module
```

## How it works:

1. **Main Function**: 
   - Reads the array size and elements from input
   - Converts string input to integer array
   - Calls merge sort function
   - Outputs the sorted array

2. **MergeSortArray Function**:
   - Recursively divides the array into halves
   - Sorts left half
   - Sorts right half
   - Merges the sorted halves

3. **Merge Function**:
   - Combines two sorted subarrays into one sorted array
   - Uses temporary arrays to store subarrays
   - Merges elements in ascending order

## Time Complexity: O(n log n)
## Space Complexity: O(n)

## Example Input:
```
10
20 18 12 19 10 1 3 2 11 17
```

## Example Output:
```
1 2 3 10 11 12 17 18 19 20
```

