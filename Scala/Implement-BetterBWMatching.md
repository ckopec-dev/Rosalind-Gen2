# Rosalind Problem: Implement_BetterBWMatching in Scala

## Problem Description
The BetterBWMatching algorithm is an optimization of the basic BWMatching algorithm for finding all occurrences of a pattern in a Burrows-Wheeler Transform (BWT) string.

## Solution

```scala
object BetterBWMatching {
  
  def betterBWMatching(bwt: String, pattern: String, firstColumn: String, lastToFirst: Array[Int]): Int = {
    val patternChars = pattern.toCharArray
    var top = 0
    var bottom = bwt.length - 1
    
    // Process pattern from right to left
    for (i <- patternChars.length - 1 to 0 by -1) {
      val symbol = patternChars(i)
      
      // Find the range of occurrences of symbol in the current range
      val symbolTop = countOccurrences(bwt, symbol, top)
      val symbolBottom = countOccurrences(bwt, symbol, bottom + 1)
      
      // If no occurrences, return 0
      if (symbolTop == symbolBottom) {
        return 0
      }
      
      // Update top and bottom
      top = firstColumn.indexOf(symbol, top) + symbolTop
      bottom = firstColumn.indexOf(symbol, bottom + 1) + symbolBottom - 1
    }
    
    // Return number of occurrences
    bottom - top + 1
  }
  
  def countOccurrences(bwt: String, symbol: Char, index: Int): Int = {
    var count = 0
    for (i <- 0 until index) {
      if (bwt(i) == symbol) {
        count += 1
      }
    }
    count
  }
  
  def buildFirstColumn(bwt: String): String = {
    val sortedBwt = bwt.sorted
    sortedBwt
  }
  
  def buildLastToFirst(bwt: String, firstColumn: String): Array[Int] = {
    val lastToFirst = new Array[Int](bwt.length)
    val firstColumnMap = new scala.collection.mutable.HashMap[Char, scala.collection.mutable.Queue[Int]]()
    
    // Initialize first column map
    for (i <- firstColumn.indices) {
      val char = firstColumn(i)
      firstColumnMap.getOrElseUpdate(char, new scala.collection.mutable.Queue[Int]()) += i
    }
    
    // Build lastToFirst mapping
    for (i <- bwt.indices) {
      val char = bwt(i)
      val firstIndex = firstColumnMap(char).dequeue()
      lastToFirst(i) = firstIndex
    }
    
    lastToFirst
  }
  
  def betterBWMatchingOptimized(bwt: String, pattern: String, firstColumn: String, lastToFirst: Array[Int]): Int = {
    var top = 0
    var bottom = bwt.length - 1
    
    // Process pattern from right to left
    for (i <- pattern.length - 1 to 0 by -1) {
      val symbol = pattern(i)
      
      // Find the range of occurrences of symbol in the current range
      val symbolTop = countOccurrences(bwt, symbol, top)
      val symbolBottom = countOccurrences(bwt, symbol, bottom + 1)
      
      // If no occurrences, return 0
      if (symbolTop == symbolBottom) {
        return 0
      }
      
      // Update top and bottom using lastToFirst mapping
      top = firstColumn.indexOf(symbol, top) + symbolTop
      bottom = firstColumn.indexOf(symbol, bottom + 1) + symbolBottom - 1
    }
    
    // Return number of occurrences
    bottom - top + 1
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val bwt = "BANANA$"
    val pattern = "ANA"
    
    val firstColumn = buildFirstColumn(bwt)
    val lastToFirst = buildLastToFirst(bwt, firstColumn)
    
    val result = betterBWMatching(bwt, pattern, firstColumn, lastToFirst)
    println(s"Number of occurrences of '$pattern' in BWT '$bwt': $result")
  }
}
```

## Alternative Implementation (More Efficient)

```scala
object BetterBWMatchingOptimized {
  
  def betterBWMatching(bwt: String, pattern: String): Int = {
    // Build first column and lastToFirst mapping
    val firstColumn = bwt.sorted
    val lastToFirst = buildLastToFirst(bwt, firstColumn)
    
    var top = 0
    var bottom = bwt.length - 1
    
    // Process pattern from right to left
    for (i <- pattern.length - 1 to 0 by -1) {
      val symbol = pattern(i)
      
      // Find first and last occurrence of symbol in current range
      val firstOccurrence = firstColumn.indexOf(symbol, top)
      val lastOccurrence = firstColumn.lastIndexOf(symbol, bottom)
      
      // If no occurrences, return 0
      if (firstOccurrence == -1 || lastOccurrence == -1 || firstOccurrence > bottom) {
        return 0
      }
      
      // Update top and bottom using lastToFirst mapping
      top = lastToFirst(firstOccurrence)
      bottom = lastToFirst(lastOccurrence)
    }
    
    // Return number of occurrences
    bottom - top + 1
  }
  
  def buildLastToFirst(bwt: String, firstColumn: String): Array[Int] = {
    val lastToFirst = new Array[Int](bwt.length)
    val firstColumnCount = new scala.collection.mutable.HashMap[Char, Int]()
    val bwtCount = new scala.collection.mutable.HashMap[Char, Int]()
    
    // Count occurrences for each character
    for (i <- bwt.indices) {
      val char = bwt(i)
      bwtCount(char) = bwtCount.getOrElse(char, 0) + 1
    }
    
    // Build mapping
    val firstColumnPositions = new scala.collection.mutable.HashMap[Char, scala.collection.mutable.Queue[Int]]()
    
    for (i <- firstColumn.indices) {
      val char = firstColumn(i)
      firstColumnPositions.getOrElseUpdate(char, new scala.collection.mutable.Queue[Int]()) += i
    }
    
    for (i <- bwt.indices) {
      val char = bwt(i)
      val firstIndex = firstColumnPositions(char).dequeue()
      lastToFirst(i) = firstIndex
    }
    
    lastToFirst
  }
  
  def main(args: Array[String]): Unit = {
    // Read input from file or console
    val bwt = scala.io.StdIn.readLine()
    val pattern = scala.io.StdIn.readLine()
    
    val result = betterBWMatching(bwt, pattern)
    println(result)
  }
}
```

## Key Points

1. **Time Complexity**: O(|Text| × |Pattern|) for the basic implementation
2. **Space Complexity**: O(|Text|) for storing auxiliary arrays
3. **Algorithm Steps**:
   - Build first column and lastToFirst mapping
   - Process pattern from right to left
   - For each symbol, find the range of occurrences in current interval
   - Update top and bottom boundaries
   - Return count of occurrences

4. **Key Components**:
   - `betterBWMatching`: Main matching function
   - `buildFirstColumn`: Creates first column from BWT
   - `buildLastToFirst`: Creates mapping from last to first column positions

This implementation efficiently finds all occurrences of a pattern in a BWT string using the BetterBWMatching algorithm.

