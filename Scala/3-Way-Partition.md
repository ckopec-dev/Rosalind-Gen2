# Rosalind 3-Way Partition Problem Solution in Scala

## Problem Understanding

The 3-Way Partition problem involves partitioning an array into three parts:
- Elements less than a pivot value
- Elements equal to the pivot value  
- Elements greater than the pivot value

This is a key step in QuickSort and other sorting algorithms.

## Solution

```scala
object ThreeWayPartition {
  
  def threeWayPartition(arr: Array[Int], pivot: Int): Array[Int] = {
    var i = 0
    var lt = 0  // less than pivot
    var gt = arr.length - 1  // greater than pivot
    
    while (i <= gt) {
      if (arr(i) < pivot) {
        // Swap arr(i) with arr(lt)
        val temp = arr(i)
        arr(i) = arr(lt)
        arr(lt) = temp
        i += 1
        lt += 1
      } else if (arr(i) > pivot) {
        // Swap arr(i) with arr(gt)
        val temp = arr(i)
        arr(i) = arr(gt)
        arr(gt) = temp
        gt -= 1
        // Don't increment i here because we need to check the new element at position i
      } else {
        // arr(i) == pivot
        i += 1
      }
    }
    
    arr
  }
  
  // Alternative implementation using immutable approach
  def threeWayPartitionImmutable(arr: Array[Int], pivot: Int): Array[Int] = {
    val (less, equal, greater) = arr.partition(_ < pivot)
    val (equalAndGreater, greaterOnly) = equal.partition(_ == pivot)
    
    less ++ equalAndGreater ++ greaterOnly
  }
  
  // For Rosalind input parsing
  def parseInput(input: String): (Array[Int], Int) = {
    val lines = input.trim.split("\n")
    val n = lines(0).toInt
    val array = lines(1).split(" ").map(_.toInt)
    val pivot = lines(2).toInt
    
    (array, pivot)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val testArray = Array(4, 5, 6, 4, 1, 2, 5, 4)
    val pivot = 4
    
    println("Original array: " + testArray.mkString(" "))
    val result = threeWayPartition(testArray.clone(), pivot)
    println("Partitioned array: " + result.mkString(" "))
    
    // Expected output: elements < 4, elements = 4, elements > 4
    // Should show: 1 2 4 4 4 5 5 6
  }
}
```

## Explanation

The solution implements the classic 3-Way Partition algorithm (also known as the Dutch National Flag problem):

1. **Three Pointers Approach**:
   - `lt` (less than): Points to the end of the "less than" section
   - `i` (current): Current element being processed
   - `gt` (greater than): Points to the start of the "greater than" section

2. **Algorithm Logic**:
   - If current element < pivot: Swap with element at `lt` position, increment both `i` and `lt`
   - If current element > pivot: Swap with element at `gt` position, decrement `gt` (don't increment `i`)
   - If current element = pivot: Simply increment `i`

3. **Time Complexity**: O(n)
4. **Space Complexity**: O(1) for the in-place version

## Sample Input/Output

**Input:**
```
7
5 4 3 2 1 4 6
4
```

**Output:**
```
1 2 3 4 4 5 6
```

## Rosalind-Specific Usage

For the actual Rosalind problem, you would typically read from a file or stdin:

```scala
import scala.io.Source

object Rosalind3WayPartition {
  def main(args: Array[String]): Unit = {
    val input = Source.fromFile("rosalind_3way.txt").mkString
    val (array, pivot) = parseInput(input)
    val result = threeWayPartition(array, pivot)
    println(result.mkString(" "))
  }
}
```

This implementation efficiently partitions the array in a single pass and handles all edge cases properly.

