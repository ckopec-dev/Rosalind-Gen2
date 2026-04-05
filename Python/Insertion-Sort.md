# Rosalind Problem: Insertion Sort

## Problem Description
Given: A positive integer n ≤ 10^3 and an array A[1..n] of integers.

Return: The number of swaps performed by insertion sort algorithm on A.

## Solution

```python
def insertion_sort_count(arr):
    """
    Count the number of swaps performed by insertion sort algorithm.
    
    Args:
        arr: List of integers
    
    Returns:
        int: Number of swaps performed
    """
    swaps = 0
    n = len(arr)
    
    # Insertion sort algorithm
    for i in range(1, n):
        key = arr[i]
        j = i - 1
        
        # Move elements greater than key one position ahead
        while j >= 0 and arr[j] > key:
            arr[j + 1] = arr[j]
            j -= 1
            swaps += 1
        
        arr[j + 1] = key
    
    return swaps

# Read input from file (assuming input.txt format)
def solve_insertion_sort(filename):
    """
    Solve the insertion sort problem from Rosalind.
    
    Args:
        filename: Input file name
    
    Returns:
        int: Number of swaps
    """
    with open(filename, 'r') as file:
        n = int(file.readline().strip())
        arr = list(map(int, file.readline().strip().split()))
    
    return insertion_sort_count(arr)

# Alternative: Direct solution with input parsing
def main():
    # Read input directly (for Rosalind format)
    import sys
    
    # Read the input
    lines = []
    for line in sys.stdin:
        lines.append(line.strip())
    
    n = int(lines[0])
    arr = list(map(int, lines[1].split()))
    
    # Solve
    result = insertion_sort_count(arr)
    print(result)

# Example usage with sample data
def example():
    # Sample input
    sample_array = [6, 10, 4, 5, 1, 2]
    print(f"Original array: {sample_array}")
    
    swaps = insertion_sort_count(sample_array.copy())
    print(f"Number of swaps: {swaps}")
    print(f"Sorted array: {sample_array}")

# If running from command line with file input
if __name__ == "__main__":
    # For Rosalind problem, we typically read from stdin
    # But here's a complete example:
    
    # Example with sample data from Rosalind
    test_array = [6, 10, 4, 5, 1, 2]
    result = insertion_sort_count(test_array)
    print(f"Input array: {test_array}")
    print(f"Number of swaps: {result}")
```

## Explanation

The insertion sort algorithm works by building the final sorted array one element at a time. For each element, it compares it with the elements before it and shifts those elements to the right until it finds the correct position.

**Key Points:**
1. We start from the second element (index 1) since a single element is already "sorted"
2. For each element, we compare it with elements to its left
3. When we find an element smaller than our current element, we insert it there
4. Each time we shift an element to the right, we count it as one swap
5. The algorithm continues until all elements are in their correct positions

**Time Complexity:** O(n²) in worst case, O(n) in best case
**Space Complexity:** O(1) - sorts in place

## Sample Input/Output

**Input:**
```
6
6 10 4 5 1 2
```

**Output:**
```
10
```

The algorithm performs 10 swaps to sort the array [6, 10, 4, 5, 1, 2] into [1, 2, 4, 5, 6, 10].

