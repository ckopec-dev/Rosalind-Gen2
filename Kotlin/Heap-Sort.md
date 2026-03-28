# Rosalind Problem: Heap Sort in Kotlin

## Problem Understanding

Heap sort is a comparison-based sorting algorithm that uses a binary heap data structure. The algorithm works by:
1. Building a max heap from the input array
2. Repeatedly extracting the maximum element and rebuilding the heap

## Solution

```kotlin
fun main() {
    // Read input from stdin
    val lines = generateSequence(::readLine).toList()
    
    // Parse the array (skip first line which contains array size)
    val array = lines[1].split(" ").map { it.toInt() }.toIntArray()
    
    // Perform heap sort
    val result = heapSort(array)
    
    // Print result
    println(result.joinToString(" "))
}

fun heapSort(array: IntArray): IntArray {
    val n = array.size
    
    // Build max heap
    for (i in (n / 2 - 1) downTo 0) {
        heapify(array, n, i)
    }
    
    // Extract elements from heap one by one
    for (i in (n - 1) downTo 1) {
        // Move current root to end
        array[0] = array[0].also { array[0] = array[i] }
        
        // Call heapify on the reduced heap
        heapify(array, i, 0)
    }
    
    return array
}

fun heapify(array: IntArray, n: Int, i: Int) {
    var largest = i
    val left = 2 * i + 1
    val right = 2 * i + 2
    
    // If left child exists and is greater than root
    if (left < n && array[left] > array[largest]) {
        largest = left
    }
    
    // If right child exists and is greater than largest so far
    if (right < n && array[right] > array[largest]) {
        largest = right
    }
    
    // If largest is not root
    if (largest != i) {
        array[i] = array[i].also { array[i] = array[largest] }
        
        // Recursively heapify the affected sub-tree
        heapify(array, n, largest)
    }
}
```

## Explanation

### Key Components:

1. **Main Function**: 
   - Reads input from standard input
   - Parses the array from the second line
   - Calls heapSort function
   - Prints the sorted result

2. **heapSort Function**:
   - First builds a max heap from the input array
   - Then repeatedly extracts the maximum element (root) and rebuilds the heap
   - Time complexity: O(n log n)

3. **heapify Function**:
   - Maintains the max heap property
   - Compares a node with its children
   - Recursively fixes the heap if necessary
   - Time complexity: O(log n)

### Algorithm Steps:

1. **Build Max Heap**: Starting from the last non-leaf node, heapify each node
2. **Extract Elements**: 
   - Swap root (maximum element) with last element
   - Reduce heap size by 1
   - Heapify the new root
3. **Repeat**: Continue until heap size becomes 1

### Time and Space Complexity:
- **Time**: O(n log n) for both best and worst cases
- **Space**: O(1) - sorts in-place

This solution efficiently handles the heap sort problem as required by Rosalind's specifications.

