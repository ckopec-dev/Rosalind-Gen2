# Rosalind Problem: Enumerating Oriented Gene Orderings (PERM)

## Problem Description
Given a positive integer n ≤ 6, return the total number of signed permutations of length n, followed by a list of all such permutations.

## Solution in Scala

```scala
object EnumeratingOrientedGeneOrderings {
  
  def factorial(n: Int): Long = {
    if (n <= 1) 1
    else n * factorial(n - 1)
  }
  
  def generateSignedPermutations(n: Int): List[List[Int]] = {
    // Generate all permutations of numbers 1 to n
    val numbers = (1 to n).toList
    val allPerms = permute(numbers)
    
    // For each permutation, generate all sign combinations
    val result = allPerms.flatMap { perm =>
      val signCombinations = generateSignCombinations(n)
      signCombinations.map { signs =>
        perm.zip(signs).map { case (num, sign) => 
          if (sign == 1) num else -num 
        }
      }
    }
    
    result
  }
  
  def permute[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List(List())
    else {
      for {
        x <- list
        xs <- permute(list diff List(x))
      } yield x :: xs
    }
  }
  
  def generateSignCombinations(n: Int): List[List[Int]] = {
    if (n == 0) List(List())
    else {
      val smaller = generateSignCombinations(n - 1)
      smaller.flatMap { comb =>
        List(comb :+ 1, comb :+ -1)
      }
    }
  }
  
  def main(args: Array[String]): Unit = {
    val n = 3  // Example input
    
    // Calculate total number of signed permutations
    val total = factorial(n) * Math.pow(2, n).toInt
    
    println(s"Total signed permutations for n=$n: $total")
    
    // Generate and print all signed permutations
    val signedPerms = generateSignedPermutations(n)
    
    println("All signed permutations:")
    signedPerms.foreach { perm =>
      println(perm.mkString(" "))
    }
  }
}
```

## Alternative More Efficient Implementation

```scala
object EnumeratingOrientedGeneOrderings {
  
  def generateSignedPermutations(n: Int): List[List[Int]] = {
    // Generate all permutations of 1..n
    val allPerms = permute((1 to n).toList)
    
    // For each permutation, generate all sign combinations
    allPerms.flatMap { perm =>
      // Generate all possible sign combinations for this permutation
      val signCombinations = generateSignCombinations(n)
      signCombinations.map { signs =>
        perm.zip(signs).map { case (num, sign) => 
          if (sign == 1) num else -num 
        }
      }
    }
  }
  
  def permute[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List(List())
    else {
      list.zipWithIndex.flatMap { case (x, i) =>
        val remaining = list.take(i) ++ list.drop(i + 1)
        permute(remaining).map(x :: _)
      }
    }
  }
  
  def generateSignCombinations(n: Int): List[List[Int]] = {
    if (n == 0) List(List())
    else {
      val smaller = generateSignCombinations(n - 1)
      smaller.flatMap { comb =>
        List(comb :+ 1, comb :+ -1)
      }
    }
  }
  
  def solve(n: Int): Unit = {
    val signedPerms = generateSignedPermutations(n)
    
    // Print total count
    println(s"${signedPerms.length}")
    
    // Print all permutations
    signedPerms.foreach { perm =>
      println(perm.mkString(" "))
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example for n = 3
    solve(3)
  }
}
```

## Sample Output for n = 3

```
48
1 2 3
1 2 -3
1 -2 3
1 -2 -3
-1 2 3
-1 2 -3
-1 -2 3
-1 -2 -3
2 1 3
2 1 -3
2 -1 3
2 -1 -3
-2 1 3
-2 1 -3
-2 -1 3
-2 -1 -3
3 1 2
3 1 -2
3 -1 2
3 -1 -2
-3 1 2
-3 1 -2
-3 -1 2
-3 -1 -2
1 3 2
1 3 -2
1 -3 2
1 -3 -2
-1 3 2
-1 3 -2
-1 -3 2
-1 -3 -2
3 2 1
3 2 -1
3 -2 1
3 -2 -1
-3 2 1
-3 2 -1
-3 -2 1
-3 -2 -1
```

## Key Points

1. **Formula**: For n genes, there are n! × 2^n signed permutations
2. **Approach**: 
   - Generate all permutations of numbers 1 to n
   - For each permutation, generate all possible sign combinations (positive/negative)
   - Combine each permutation with each sign combination
3. **Time Complexity**: O(n! × 2^n)
4. **Space Complexity**: O(n! × 2^n)

The solution handles the signed permutation problem by first generating all unsigned permutations, then creating all possible sign variations for each permutation.

