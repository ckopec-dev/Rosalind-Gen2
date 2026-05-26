# Rosalind Problem: Building a Heap (BAUM)

## Problem Description
Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: An array A′[1..n] such that it is a heap (i.e., for every i, A[i] ≤ A[2i] and A[i] ≤ A[2i+1], assuming that both 2i and 2i+1 are at most n).

## Solution Approach
We need to convert an array into a min-heap using the heapify process. The key insight is that we can build a heap by starting from the last non-leaf node and heapifying each node downward.

## C Implementation

```c
#include <stdio.h>
#include <stdlib.h>

// Function to heapify a subtree rooted at index i
void heapify(int arr[], int n, int i) {
    int smallest = i;      // Initialize smallest as root
    int left = 2 * i + 1;  // Left child
    int right = 2 * i + 2; // Right child
    
    // If left child exists and is smaller than root
    if (left < n && arr[left] < arr[smallest])
        smallest = left;
    
    // If right child exists and is smaller than smallest so far
    if (right < n && arr[right] < arr[smallest])
        smallest = right;
    
    // If smallest is not root
    if (smallest != i) {
        // Swap
        int temp = arr[i];
        arr[i] = arr[smallest];
        arr[smallest] = temp;
        
        // Recursively heapify the affected sub-tree
        heapify(arr, n, smallest);
    }
}

// Function to build a heap from array
void buildHeap(int arr[], int n) {
    // Start from last non-leaf node and heapify each node
    for (int i = n / 2 - 1; i >= 0; i--) {
        heapify(arr, n, i);
    }
}

int main() {
    int n;
    scanf("%d", &n);
    
    int *arr = (int*)malloc(n * sizeof(int));
    
    // Read array elements
    for (int i = 0; i < n; i++) {
        scanf("%d", &arr[i]);
    }
    
    // Build heap
    buildHeap(arr, n);
    
    // Print result
    for (int i = 0; i < n; i++) {
        printf("%d ", arr[i]);
    }
    printf("\n");
    
    free(arr);
    return 0;
}
```

## Explanation

1. **Heapify Function**: This function ensures that the subtree rooted at index `i` satisfies the min-heap property. It compares the root with its children and swaps if necessary, then recursively heapifies the affected subtree.

2. **BuildHeap Function**: Starting from the last non-leaf node (at index `n/2 - 1`), we heapify each node downward. This approach works because:
   - All leaf nodes are already heaps
   - We process nodes in reverse level order to ensure proper heapification

3. **Main Function**: 
   - Reads the input size `n`
   - Reads the array elements
   - Builds the heap using `buildHeap()`
   - Prints the resulting heap

## Time Complexity
- **Time**: O(n) - Building a heap from an array takes linear time
- **Space**: O(log n) - Due to recursion stack depth in heapify

## Example
Input:
```
5
1 3 5 7 2
```

Output:
```
1 2 5 7 3
```

The algorithm transforms the input array into a min-heap where each parent node is smaller than or equal to its children.

