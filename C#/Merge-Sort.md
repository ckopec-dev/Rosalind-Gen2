# Rosalind Problem: Merge Sort Solution in C#

## Problem Understanding

The merge sort problem requires implementing the merge sort algorithm to sort an array of integers in ascending order.

## Solution

```csharp
using System;
using System.IO;
using System.Linq;

class Program
{
    static void Main(string[] args)
    {
        // Read input from file
        string input = File.ReadAllText("input.txt");
        string[] lines = input.Split(new char[] { '\n' }, StringSplitOptions.RemoveEmptyEntries);
        
        // Parse the array
        int[] array = lines[1].Split(' ').Select(int.Parse).ToArray();
        
        // Perform merge sort
        int[] sortedArray = MergeSort(array);
        
        // Output result
        Console.WriteLine(string.Join(" ", sortedArray));
    }
    
    static int[] MergeSort(int[] array)
    {
        if (array.Length <= 1)
            return array;
            
        int mid = array.Length / 2;
        int[] left = array.Take(mid).ToArray();
        int[] right = array.Skip(mid).ToArray();
        
        left = MergeSort(left);
        right = MergeSort(right);
        
        return Merge(left, right);
    }
    
    static int[] Merge(int[] left, int[] right)
    {
        int[] result = new int[left.Length + right.Length];
        int i = 0, j = 0, k = 0;
        
        // Merge the two sorted arrays
        while (i < left.Length && j < right.Length)
        {
            if (left[i] <= right[j])
            {
                result[k] = left[i];
                i++;
            }
            else
            {
                result[k] = right[j];
                j++;
            }
            k++;
        }
        
        // Copy remaining elements from left array
        while (i < left.Length)
        {
            result[k] = left[i];
            i++;
            k++;
        }
        
        // Copy remaining elements from right array
        while (j < right.Length)
        {
            result[k] = right[j];
            j++;
            k++;
        }
        
        return result;
    }
}
```

## Algorithm Explanation

1. **Base Case**: If the array has 1 or 0 elements, it's already sorted
2. **Divide**: Split the array into two halves
3. **Conquer**: Recursively sort both halves
4. **Combine**: Merge the two sorted halves back together

## Time and Space Complexity

- **Time Complexity**: O(n log n) - The array is divided log n times, and each merge operation takes O(n) time
- **Space Complexity**: O(n) - Additional space is needed for the temporary arrays during merging

## Sample Input/Output

**Input:**
```
10
20 18 12 19 10 1 5 2 17 13
```

**Output:**
```
1 2 5 10 12 13 17 18 19 20
```

## Key Features

- Uses LINQ methods for clean array manipulation
- Handles edge cases (empty arrays, single elements)
- Implements the classic divide-and-conquer merge sort algorithm
- Properly merges two sorted subarrays into one sorted array
- Reads input from file and outputs to console as required by Rosalind format

