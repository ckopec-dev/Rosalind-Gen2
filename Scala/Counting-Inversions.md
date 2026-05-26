# Rosalind Problem: Counting Inversions in Scala

## Problem Understanding

An inversion in an array A[1..n] is a pair of indices (i, j) such that i < j and A[i] > A[j]. The task is to count the number of inversions in a given array.

## Solution Approach

I'll use a modified merge sort algorithm to count inversions efficiently in O(n log n) time complexity.

## Scala Implementation

```scala
object CountingInversions {
  
  def mergeAndCount(arr: Array[Int], tempArr: Array[Int], left: Int, mid: Int, right: Int): Long = {
    var i = left
    var j = mid + 1
    var k = left
    var invCount = 0L
    
    // Merge the two subarrays while counting inversions
    while (i <= mid && j <= right) {
      if (arr(i) <= arr(j)) {
        tempArr(k) = arr(i)
        i += 1
      } else {
        tempArr(k) = arr(j)
        // All elements from i to mid are greater than arr(j)
        invCount += (mid - i + 1)
        j += 1
      }
      k += 1
    }
    
    // Copy remaining elements
    while (i <= mid) {
      tempArr(k) = arr(i)
      i += 1
      k += 1
    }
    
    while (j <= right) {
      tempArr(k) = arr(j)
      j += 1
      k += 1
    }
    
    // Copy back the merged elements
    for (i <- left to right) {
      arr(i) = tempArr(i)
    }
    
    invCount
  }
  
  def mergeSortAndCount(arr: Array[Int], tempArr: Array[Int], left: Int, right: Int): Long = {
    var invCount = 0L
    
    if (left < right) {
      val mid = left + (right - left) / 2
      
      invCount += mergeSortAndCount(arr, tempArr, left, mid)
      invCount += mergeSortAndCount(arr, tempArr, mid + 1, right)
      invCount += mergeAndCount(arr, tempArr, left, mid, right)
    }
    
    invCount
  }
  
  def countInversions(arr: Array[Int]): Long = {
    val tempArr = new Array[Int](arr.length)
    mergeSortAndCount(arr, tempArr, 0, arr.length - 1)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val inputArray = Array(1, 3, 5, 2, 4, 6)
    val result = countInversions(inputArray)
    println(s"Number of inversions: $result")
    
    // Test with the sample from Rosalind
    val rosalindInput = Array(1, 3, 5, 2, 4, 6)
    val rosalindResult = countInversions(rosalindInput)
    println(s"Rosalind test result: $rosalindResult")
  }
}
```

## Alternative Simpler Approach (Less Efficient)

```scala
object CountingInversionsSimple {
  
  def countInversions(arr: Array[Int]): Long = {
    var count = 0L
    for (i <- arr.indices) {
      for (j <- i + 1 until arr.length) {
        if (arr(i) > arr(j)) {
          count += 1
        }
      }
    }
    count
  }
  
  def main(args: Array[String]): Unit = {
    val input = Array(1, 3, 5, 2, 4, 6)
    val result = countInversions(input)
    println(s"Number of inversions: $result")
  }
}
```

## Explanation

### Efficient Approach (Merge Sort):
1. **Divide**: Split the array into two halves
2. **Conquer**: Recursively count inversions in both halves
3. **Combine**: Count inversions between the two halves during the merge process
4. **Key Insight**: When merging, if an element from the right subarray is smaller than an element from the left subarray, it forms inversions with all remaining elements in the left subarray

### Time Complexity:
- **Efficient**: O(n log n) - Uses merge sort approach
- **Simple**: O(n²) - Nested loops approach

### Space Complexity:
- **Efficient**: O(n) - For temporary array
- **Simple**: O(1) - No extra space needed

## Test Case

For input array `[1, 3, 5, 2, 4, 6]`:
- Inversions: (3,2), (5,2), (5,4) = 3 inversions

The efficient merge sort approach is recommended for larger inputs as it scales better with input size.

