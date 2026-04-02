# Rosalind Problem: Enumerating Gene Orders (PERM)

## Problem Description
Given a positive integer n ≤ 7, return the total number of permutations of length n, followed by a list of all such permutations (in any order).

## Solution in Scala

```scala
object EnumeratingGeneOrders {
  def main(args: Array[String]): Unit = {
    val n = 3  // Example input
    
    // Generate all permutations
    val permutations = permute(1 to n)
    
    // Print the total number of permutations
    println(permutations.length)
    
    // Print each permutation
    permutations.foreach(p => println(p.mkString(" ")))
  }
  
  def permute(range: Range): List[List[Int]] = {
    if (range.isEmpty) {
      List(List())
    } else {
      for {
        i <- range
        rest <- permute(range.filter(_ != i))
      } yield i :: rest
    }
  }
}
```

## Alternative Implementation Using Built-in Methods

```scala
object EnumeratingGeneOrders {
  def main(args: Array[String]): Unit = {
    val n = 3  // Example input
    
    // Generate all permutations using Scala's built-in methods
    val numbers = (1 to n).toList
    val permutations = numbers.permutations.toList
    
    // Print the total number of permutations
    println(permutations.length)
    
    // Print each permutation
    permutations.foreach(p => println(p.mkString(" ")))
  }
}
```

## More Efficient Implementation

```scala
object EnumeratingGeneOrders {
  def main(args: Array[String]): Unit = {
    val n = 3  // Example input
    
    // Generate all permutations
    val permutations = generatePermutations(n)
    
    // Print the total number of permutations
    println(permutations.length)
    
    // Print each permutation
    permutations.foreach(p => println(p.mkString(" ")))
  }
  
  def generatePermutations(n: Int): List[List[Int]] = {
    if (n == 1) {
      List(List(1))
    } else {
      val smallerPerms = generatePermutations(n - 1)
      smallerPerms.flatMap(perm => 
        (0 to perm.length).map(i => 
          perm.take(i) ++ List(n) ++ perm.drop(i)
        )
      )
    }
  }
}
```

## Complete Working Solution

```scala
object EnumeratingGeneOrders {
  def main(args: Array[String]): Unit = {
    val n = 3  // Input value
    
    // Generate all permutations
    val permutations = permute(1 to n)
    
    // Print total count
    println(permutations.length)
    
    // Print each permutation
    permutations.foreach(perm => println(perm.mkString(" ")))
  }
  
  def permute(range: Range): List[List[Int]] = {
    if (range.isEmpty) {
      List(List())
    } else {
      range.flatMap(i => 
        permute(range.filter(_ != i)).map(i :: _)
      ).toList
    }
  }
}
```

## Example Output for n = 3

```
6
1 2 3
1 3 2
2 1 3
2 3 1
3 1 2
3 2 1
```

## Explanation

1. **Problem Understanding**: We need to generate all possible permutations of numbers from 1 to n
2. **Approach**: 
   - Generate all permutations recursively
   - For each number, place it at the front and recursively generate permutations of the remaining numbers
3. **Time Complexity**: O(n! × n) where n! is the number of permutations and n is the time to copy each permutation
4. **Space Complexity**: O(n! × n) for storing all permutations

The solution uses a recursive approach where we:
- Take each element in the range
- Place it at the beginning
- Recursively generate permutations of the remaining elements
- Combine them to form complete permutations

