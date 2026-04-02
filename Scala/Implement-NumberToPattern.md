# Rosalind Problem: Implement_NumberToPattern

## Problem Description
Implement the NumberToPattern function that converts a number to its corresponding DNA pattern of a given length.

## Solution

```scala
def numberToPattern(index: Long, k: Int): String = {
  if (k == 1) {
    index match {
      case 0 => "A"
      case 1 => "C"
      case 2 => "G"
      case 3 => "T"
    }
  } else {
    val prefixIndex = index / 4
    val remainder = (index % 4).toInt
    val prefixPattern = numberToPattern(prefixIndex, k - 1)
    
    remainder match {
      case 0 => prefixPattern + "A"
      case 1 => prefixPattern + "C"
      case 2 => prefixPattern + "G"
      case 3 => prefixPattern + "T"
    }
  }
}

// Alternative iterative implementation
def numberToPatternIterative(index: Long, k: Int): String = {
  val symbols = Array("A", "C", "G", "T")
  val pattern = new StringBuilder
  
  var currentIndex = index
  for (i <- 0 until k) {
    pattern.append(symbols((currentIndex % 4).toInt))
    currentIndex = currentIndex / 4
  }
  
  pattern.reverse.toString
}

// Test function
def testNumberToPattern(): Unit = {
  println(numberToPattern(45, 4))  // Should output "ACGT"
  println(numberToPattern(0, 1))   // Should output "A"
  println(numberToPattern(3, 1))   // Should output "T"
  println(numberToPattern(11, 2))  // Should output "GT"
}

// Example usage
val result = numberToPattern(45, 4)
println(result)  // Output: ACGT
```

## Explanation

The NumberToPattern function converts a number (index) to its corresponding DNA pattern of length k. This is essentially converting a number from base 10 to base 4, where each digit represents a nucleotide:

- 0 → 'A'
- 1 → 'C' 
- 2 → 'G'
- 3 → 'T'

### Recursive Approach:
1. Base case: If k = 1, directly map the index to a nucleotide
2. Recursive case: 
   - Divide the index by 4 to get the prefix pattern index
   - Use the remainder to determine the last nucleotide
   - Recursively get the prefix pattern and append the nucleotide

### Iterative Approach:
1. Use an array to map digits to nucleotides
2. Repeatedly divide the index by 4 and build the pattern from right to left
3. Reverse the final pattern to get the correct order

## Time and Space Complexity
- **Time Complexity**: O(k) where k is the pattern length
- **Space Complexity**: O(k) for the recursive call stack (recursive) or O(k) for the iterative approach

## Sample Input/Output
```
Input: index = 45, k = 4
Output: "ACGT"

Input: index = 0, k = 1  
Output: "A"

Input: index = 11, k = 2
Output: "GT"
```

