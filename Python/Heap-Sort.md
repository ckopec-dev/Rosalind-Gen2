# Rosalind Problem: Heap Sort

## Problem Description
Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: A sorted array A.

## Solution Approach
I'll implement heap sort using a max heap. The algorithm works by:
1. Building a max heap from the input array
2. Repeatedly extracting the maximum element and placing it at the end
3. Heapifying the remaining elements

## Python Implementation

```python
def heapify(arr, n, i):
    """
    Heapify subtree rooted at index i
    n is size of heap
    """
    largest = i  # Initialize largest as root
    left = 2 * i + 1     # left child
    right = 2 * i + 2    # right child
    
    # If left child exists and is greater than root
    if left < n and arr[left] > arr[largest]:
        largest = left
    
    # If right child exists and is greater than largest so far
    if right < n and arr[right] > arr[largest]:
        largest = right
    
    # If largest is not root
    if largest != i:
        arr[i], arr[largest] = arr[largest], arr[i]
        heapify(arr, n, largest)

def heap_sort(arr):
    """
    Main function to sort array using heap sort
    """
    n = len(arr)
    
    # Build a max heap
    for i in range(n // 2 - 1, -1, -1):
        heapify(arr, n, i)
    
    # Extract elements from heap one by one
    for i in range(n - 1, 0, -1):
        # Move current root to end
        arr[0], arr[i] = arr[i], arr[0]
        
        # Call heapify on the reduced heap
        heapify(arr, i, 0)
    
    return arr

# Read input and solve
def solve_heap_sort():
    # Read input from stdin
    import sys
    input_lines = sys.stdin.read().strip().split('\n')
    
    n = int(input_lines[0])
    array = list(map(int, input_lines[1].split()))
    
    # Sort the array using heap sort
    sorted_array = heap_sort(array)
    
    # Print result
    print(' '.join(map(str, sorted_array)))

# Example usage (for testing)
def example():
    # Example from problem
    n = 5
    array = [1, 3, 5, 7, 2]
    
    print("Original array:", array)
    sorted_array = heap_sort(array)
    print("Sorted array:", sorted_array)

# Run the solution
if __name__ == "__main__":
    solve_heap_sort()
```

## How it works:

1. **Heapify function**: Maintains the max heap property by ensuring that the largest element is at the root
2. **Heap sort function**: 
   - First builds a max heap from the input array
   - Then repeatedly extracts the maximum element (root) and places it at the end
   - After each extraction, heapifies the remaining elements

## Time Complexity: O(n log n)
## Space Complexity: O(1) (in-place sorting)

## Sample Input/Output:
```
Input:
5
1 3 5 7 2

Output:
1 2 3 5 7
```

The solution handles the Rosalind problem requirements by reading the input array and returning it sorted in ascending order using the heap sort algorithm.

