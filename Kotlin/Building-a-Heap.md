# Rosalind Problem: Building a Heap (Building_a_Heap)

## Problem Description
Given: A positive integer n ≤ 10^5 and array A[1..n] of integers from -10^5 to 10^5.

Return: A heap obtained through the build-heap operation applied to A.

## Solution Approach
To build a heap from an array, we need to perform the "heapify" operation starting from the last non-leaf node and working backwards to the root. For an array of size n, the last non-leaf node is at index n/2 - 1 (0-indexed).

## Kotlin Implementation

```kotlin
fun buildHeap(arr: IntArray): IntArray {
    val n = arr.size
    
    // Start from the last non-leaf node and heapify each node
    for (i in (n / 2 - 1) downTo 0) {
        heapify(arr, n, i)
    }
    
    return arr
}

fun heapify(arr: IntArray, n: Int, i: Int) {
    var largest = i
    val left = 2 * i + 1
    val right = 2 * i + 2
    
    // If left child exists and is greater than root
    if (left < n && arr[left] > arr[largest]) {
        largest = left
    }
    
    // If right child exists and is greater than largest so far
    if (right < n && arr[right] > arr[largest]) {
        largest = right
    }
    
    // If largest is not root
    if (largest != i) {
        arr[i] = arr[i] xor arr[largest]
        arr[largest] = arr[i] xor arr[largest]
        arr[i] = arr[i] xor arr[largest]
        
        // Recursively heapify the affected sub-tree
        heapify(arr, n, largest)
    }
}

fun main() {
    // Read input
    val n = readLine()!!.toInt()
    val arr = readLine()!!.split(" ").map { it.toInt() }.toIntArray()
    
    // Build heap
    val heap = buildHeap(arr)
    
    // Print result
    println(heap.joinToString(" "))
}
```

## Explanation

1. **Build Heap Function**: 
   - Start from the last non-leaf node (index n/2 - 1)
   - Work backwards to the root (index 0)
   - Apply heapify operation to each node

2. **Heapify Function**:
   - Compare parent with its children
   - Find the largest among parent and children
   - If parent is not the largest, swap with the largest child
   - Recursively heapify the affected subtree

3. **Swap Operation**:
   - Uses XOR swap technique to swap elements without temporary variable
   - This is a clever way to avoid using extra space

## Time Complexity
- **Build Heap**: O(n) time complexity
- **Heapify**: O(log n) for each node
- **Overall**: O(n) since we do heapify operations on approximately n/2 nodes

## Space Complexity
- O(log n) due to recursion stack depth in heapify

## Example
Input:
```
5
1 3 5 7 2
```

Output:
```
7 3 5 1 2
```

This represents a max heap where each parent node is greater than or equal to its children.

