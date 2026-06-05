# Rosalind Problem: Majority Element

## Problem Description
Given an array of positive integers, find the majority element - the element that appears more than ⌊n/2⌋ times. It is guaranteed that a majority element exists in the input.

## Solution in C#

```csharp
using System;
using System.Collections.Generic;
using System.Linq;

public class MajorityElement
{
    public static int FindMajorityElement(int[] array)
    {
        // Using Boyer-Moore Majority Vote Algorithm
        int candidate = 0;
        int count = 0;
        
        // Phase 1: Find candidate
        foreach (int num in array)
        {
            if (count == 0)
                candidate = num;
            
            count += (num == candidate) ? 1 : -1;
        }
        
        // Phase 2: Verify candidate is actually majority element
        // (This step is optional since problem guarantees majority element exists)
        count = 0;
        foreach (int num in array)
        {
            if (num == candidate)
                count++;
        }
        
        if (count > array.Length / 2)
            return candidate;
            
        return -1; // Should never reach here given problem constraints
    }
    
    // Alternative solution using dictionary counting
    public static int FindMajorityElementAlternative(int[] array)
    {
        Dictionary<int, int> counts = new Dictionary<int, int>();
        int n = array.Length;
        
        foreach (int num in array)
        {
            if (counts.ContainsKey(num))
                counts[num]++;
            else
                counts[num] = 1;
                
            if (counts[num] > n / 2)
                return num;
        }
        
        return -1;
    }
    
    public static void Main(string[] args)
    {
        // Example usage
        int[] array1 = {3, 2, 3};
        Console.WriteLine($"Majority element in [3, 2, 3]: {FindMajorityElement(array1)}");
        
        int[] array2 = {2, 2, 1, 1, 1, 2, 2};
        Console.WriteLine($"Majority element in [2, 2, 1, 1, 1, 2, 2]: {FindMajorityElement(array2)}");
        
        int[] array3 = {1};
        Console.WriteLine($"Majority element in [1]: {FindMajorityElement(array3)}");
    }
}
```

## Explanation

### Approach 1: Boyer-Moore Majority Vote Algorithm
- **Time Complexity**: O(n)
- **Space Complexity**: O(1)
- This is the optimal solution for finding majority elements
- The algorithm works in two phases:
  1. **Phase 1**: Find a candidate for majority element by maintaining a count
  2. **Phase 2**: Verify that the candidate is actually the majority element

### Approach 2: Hash Map Counting
- **Time Complexity**: O(n)
- **Space Complexity**: O(n)
- Uses a dictionary to count occurrences of each element
- Returns immediately when an element exceeds n/2 occurrences

## Key Points

1. **Boyer-Moore Algorithm**: More space-efficient, uses constant extra space
2. **Dictionary Approach**: More intuitive, easier to understand
3. **Guaranteed Solution**: The problem states that a majority element always exists
4. **Edge Cases**: Handles single element arrays correctly

## Sample Input/Output

```
Input: [3, 2, 3]
Output: 3

Input: [2, 2, 1, 1, 1, 2, 2]
Output: 2

Input: [1]
Output: 1
```

The Boyer-Moore algorithm is preferred for its optimal space complexity, making it suitable for large datasets.

