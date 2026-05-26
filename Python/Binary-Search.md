# Rosalind Problem: Binary Search

## Problem Description
Given: Two positive integers n ≤ 10⁵ and m ≤ 10⁵, followed by a sorted array A[1..n] of integers from −10⁵ to 10⁵ and m integers from −10⁵ to 10⁵.

Return: For each integer k in the second array, output the index of k in array A (1-indexed), or -1 if k is not found.

## Solution

```python
def binary_search(arr, target):
    """
    Perform binary search on a sorted array to find target element.
    Returns 1-indexed position if found, -1 otherwise.
    """
    left, right = 0, len(arr) - 1
    
    while left <= right:
        mid = (left + right) // 2
        
        if arr[mid] == target:
            return mid + 1  # Convert to 1-indexed
        elif arr[mid] < target:
            left = mid + 1
        else:
            right = mid - 1
    
    return -1

def solve_binary_search(n, m, array, targets):
    """
    Solve the binary search problem.
    """
    results = []
    for target in targets:
        result = binary_search(array, target)
        results.append(str(result))
    
    return ' '.join(results)

# Read input
def main():
    # Read the input file or use sample data
    # For Rosalind, you would typically read from a file
    # Here's the sample input format:
    
    # Sample input:
    # 5 6
    # 1 2 3 4 5
    # 1 2 3 4 5 6
    
    # For demonstration, let's parse from input
    import sys
    
    # Read all lines
    lines = [line.strip() for line in sys.stdin if line.strip()]
    
    # Parse n and m
    n, m = map(int, lines[0].split())
    
    # Parse sorted array A
    array = list(map(int, lines[1].split()))
    
    # Parse target values
    targets = list(map(int, lines[2].split()))
    
    # Solve and output
    result = solve_binary_search(n, m, array, targets)
    print(result)

# Alternative implementation for direct testing
def test_binary_search():
    """
    Test the binary search implementation with sample data.
    """
    # Sample input
    n, m = 5, 6
    array = [1, 2, 3, 4, 5]
    targets = [1, 2, 3, 4, 5, 6]
    
    # Expected output: 1 2 3 4 5 -1
    results = []
    for target in targets:
        result = binary_search(array, target)
        results.append(str(result))
    
    return ' '.join(results)

# Run test
if __name__ == "__main__":
    # For actual Rosalind submission, you would read from file
    # For testing purposes:
    print("Test result:", test_binary_search())
```

## Explanation

### Algorithm Steps:
1. **Read Input**: Parse n, m, the sorted array A, and the target values
2. **Binary Search Function**: 
   - Initialize left and right pointers
   - While left ≤ right:
     - Calculate mid point
     - If mid element equals target, return 1-indexed position
     - If mid element < target, search right half
     - If mid element > target, search left half
     - If not found, return -1
3. **Process All Targets**: Apply binary search to each target value
4. **Output Results**: Return space-separated results

### Time Complexity:
- **Binary Search**: O(log n) for each search
- **Overall**: O(m log n) where m is number of targets

### Space Complexity:
- O(1) additional space (excluding input storage)

## Sample Input/Output

**Input:**
```
5 6
1 2 3 4 5
1 2 3 4 5 6
```

**Output:**
```
1 2 3 4 5 -1
```

The algorithm efficiently finds each target in the sorted array using binary search, which is much faster than linear search for large datasets.

