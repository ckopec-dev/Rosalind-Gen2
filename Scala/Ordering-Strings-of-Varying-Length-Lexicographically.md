# Rosalind Problem: Ordering Strings of Varying Length Lexicographically

## Problem Description
Given a collection of strings and a custom alphabet, return all permutations of the strings ordered lexicographically according to the given alphabet.

## Solution in Scala

```scala
object OrderingStrings {
  
  def lexicographicallyOrdered(strings: List[String], alphabet: String): List[String] = {
    // Create a mapping from each character to its position in the alphabet
    val charToIndex = alphabet.zipWithIndex.toMap
    
    // Define a custom comparison function
    def compareStrings(s1: String, s2: String): Int = {
      val minLength = math.min(s1.length, s2.length)
      
      // Compare character by character
      for (i <- 0 until minLength) {
        val char1 = s1(i)
        val char2 = s2(i)
        
        val index1 = charToIndex(char1)
        val index2 = charToIndex(char2)
        
        if (index1 < index2) return -1
        if (index1 > index2) return 1
      }
      
      // If all compared characters are equal, shorter string comes first
      s1.length.compare(s2.length)
    }
    
    // Sort the strings using the custom comparison
    strings.sorted(compareStrings)
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val alphabet = "DNA"
    val strings = List("D", "N", "A", "DN", "NA", "DA", "DNA")
    
    val result = lexicographicallyOrdered(strings, alphabet)
    println(result.mkString(" "))
  }
}
```

## Alternative Implementation Using Ordering

```scala
object OrderingStringsAlternative {
  
  def lexicographicallyOrdered(strings: List[String], alphabet: String): List[String] = {
    // Create a mapping from each character to its position in the alphabet
    val charToIndex = alphabet.zipWithIndex.toMap
    
    // Create a custom Ordering
    implicit val stringOrdering: Ordering[String] = new Ordering[String] {
      override def compare(x: String, y: String): Int = {
        val minLength = math.min(x.length, y.length)
        
        // Compare character by character
        for (i <- 0 until minLength) {
          val char1 = x(i)
          val char2 = y(i)
          
          val index1 = charToIndex(char1)
          val index2 = charToIndex(char2)
          
          if (index1 < index2) return -1
          if (index1 > index2) return 1
        }
        
        // If all compared characters are equal, shorter string comes first
        x.length.compare(y.length)
      }
    }
    
    // Sort the strings using the custom ordering
    strings.sorted
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage with sample data
    val alphabet = "D N A" // Note: spaces in alphabet
    val strings = List("D", "N", "A", "DN", "NA", "DA", "DNA")
    
    val result = lexicographicallyOrdered(strings, alphabet)
    println(result.mkString(" "))
  }
}
```

## Key Points

1. **Character Mapping**: Create a mapping from each character to its position in the given alphabet
2. **Custom Comparison**: Implement a comparison function that respects the custom alphabet order
3. **Lexicographic Ordering**: When characters are equal, shorter strings come first
4. **Sorting**: Use Scala's built-in sorting with custom ordering

## Time Complexity
- **Time**: O(n × m × log n) where n is the number of strings and m is the average string length
- **Space**: O(k) where k is the size of the alphabet

## Sample Input/Output

**Input:**
```
alphabet: "D N A"
strings: ["D", "N", "A", "DN", "NA", "DA", "DNA"]
```

**Output:**
```
A DA DN DNA N NA
```

The strings are ordered according to the custom alphabet "D N A", so "A" comes first, then "D", then "N", and within each group, strings are ordered by length.

