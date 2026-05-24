# Rosalind Problem: Building a Heap (BAW)

## Problem Statement
Given: A positive integer n ≤ 10^5 and an array A[1..n] of integers from -10^5 to 10^5.

Return: An array A′[1..n] such that it is a max heap. (A max heap is a complete binary tree where each node is greater than or equal to its children.)

## Solution Approach
We need to build a max heap from an unsorted array. The algorithm works by:
1. Starting from the last non-leaf node (at index n/2 - 1)
2. Applying the heapify operation to each node moving upwards
3. The heapify operation ensures the max heap property is maintained

## Scala Implementation

```scala
object BuildingAHeap {
  def main(args: Array[String]): Unit = {
    // Read input
    val lines = scala.io.Source.fromFile("rosalind_baw.txt").getLines().toList
    val n = lines(0).toInt
    val array = lines(1).split(" ").map(_.toInt)
    
    // Build max heap
    val result = buildMaxHeap(array)
    
    // Output result
    println(result.mkString(" "))
  }
  
  def buildMaxHeap(array: Array[Int]): Array[Int] = {
    val n = array.length
    val heap = array.clone()
    
    // Start from the last non-leaf node and heapify each node
    for (i <- (n / 2 - 1) to 0 by -1) {
      heapify(heap, n, i)
    }
    
    heap
  }
  
  def heapify(heap: Array[Int], n: Int, i: Int): Unit = {
    var largest = i
    val left = 2 * i + 1
    val right = 2 * i + 2
    
    // If left child exists and is greater than root
    if (left < n && heap(left) > heap(largest)) {
      largest = left
    }
    
    // If right child exists and is greater than largest so far
    if (right < n && heap(right) > heap(largest)) {
      largest = right
    }
    
    // If largest is not root
    if (largest != i) {
      // Swap
      val temp = heap(i)
      heap(i) = heap(largest)
      heap(largest) = temp
      
      // Recursively heapify the affected sub-tree
      heapify(heap, n, largest)
    }
  }
}
```

## Example

**Input:**
```
5
1 3 5 7 2
```

**Output:**
```
7 3 5 1 2
```

## Explanation

1. **Initial array:** [1, 3, 5, 7, 2]
2. **Start from index 1 (n/2 - 1 = 2):**
   - Heapify index 2: 5 is already greater than its children
   - Heapify index 1: 3 < 7, so swap 3 and 7 → [1, 7, 5, 3, 2]
   - Heapify index 0: 1 < 7, so swap 1 and 7 → [7, 1, 5, 3, 2]
3. **Final result:** [7, 3, 5, 1, 2] (max heap property maintained)

## Time Complexity
- **Time:** O(n) - Building a heap from an array takes linear time
- **Space:** O(log n) - Due to recursive calls in heapify

## Key Points
- The algorithm starts from the last non-leaf node (n/2 - 1) and works upward
- Heapify ensures the max heap property is maintained for each subtree
- This is an in-place algorithm that modifies the original array
- The result is a valid max heap where each parent node is greater than or equal to its children

