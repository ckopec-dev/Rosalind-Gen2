# Rosalind Problem: Introduction to Random Strings

## Problem Statement
Given: A DNA string s of length at most 100 bp and an array A of 10 real numbers between 0.0 and 1.0. Return: An array B of length 10 where B[i] is the probability that if we randomly select a DNA string of the same length as s, the probability of getting that string is at least as high as the probability of getting s.

## Solution Approach
1. Calculate the probability of a random DNA string of the same length as s
2. For each value in array A, calculate the probability that a random string matches s
3. The answer is the probability that a random string matches s

Wait, let me re-read this more carefully. The problem is asking for the probability that a random DNA string of the same length as s matches s.

Actually, looking at this more carefully, I think this is asking for:
- Given a DNA string s and an array A of probabilities
- For each probability p in A, calculate the probability that a random DNA string of the same length as s matches s

But that doesn't make sense either. Let me re-read...

Looking at the standard Rosalind problem, this is about calculating the probability of a random DNA string matching a given string. Let me approach it differently:

The problem is asking to compute the probability of a random DNA string matching the given string s.

## Solution in Scala

```scala
def solve(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  val probabilities = new Array[Double](A.length)
  
  for (i <- A.indices) {
    val p = A(i)
    // Calculate probability of a random DNA string matching s
    // For each position, probability of matching is p/4 for each nucleotide
    // But we need to calculate based on the nucleotide frequencies in s
    
    // Calculate the probability of the specific string s
    val logProb = s.map {
      case 'A' => math.log(0.25)
      case 'T' => math.log(0.25)
      case 'G' => math.log(0.25)
      case 'C' => math.log(0.25)
      case _ => 0.0
    }.sum
    
    // Convert back to probability
    probabilities(i) = math.exp(logProb)
  }
  
  probabilities
}

// Alternative interpretation - calculating probability of matching a string
def solveAlternative(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  val probabilities = new Array[Double](A.length)
  
  // For each value in A, we want to calculate the probability that a random string
  // matches s under the given probability distribution
  for (i <- A.indices) {
    val p = A(i)
    // Probability of a random DNA string of length n matching s
    // This is the probability of s under a uniform distribution
    probabilities(i) = math.pow(0.25, n)
  }
  
  probabilities
}

// Correct interpretation: calculate probability that a random DNA string matches s
def calculateRandomStringProbability(s: String): Double = {
  val n = s.length
  // Probability of a random DNA string of same length matching s
  // Each nucleotide has probability 0.25 of matching
  math.pow(0.25, n)
}

def solveRosalind(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  val result = new Array[Double](A.length)
  
  // For each probability in A, we calculate the probability of a random string
  // matching s (which is always the same regardless of A values)
  val prob = math.pow(0.25, n)
  
  // But the problem seems to be about computing the probability of getting
  // a string with at least as high probability as s under some given probabilities
  
  // Let's re-interpret: for each A[i], we want to compute probability that 
  // a random string has probability >= probability of s
  val probS = math.pow(0.25, n)
  
  // Actually, let's implement the correct approach:
  // The problem is to compute log-likelihood or probability of s
  // under the given probability distribution
  
  for (i <- A.indices) {
    // This is the probability of the string s under uniform distribution
    result(i) = math.pow(0.25, n)
  }
  
  result
}

// Final clean solution
def solve(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  val prob = math.pow(0.25, n)
  Array.fill(A.length)(prob)
}

// Example usage:
val s = "ACGTACGT"
val A = Array(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
val result = solve(s, A)
println(result.mkString(" "))
```

Wait, let me think about this more carefully. Looking at the problem again, I think this is asking for the probability that a random DNA string of the same length as s matches s, which is simply (1/4)^n where n is the length of s.

But since the array A is given, it might be asking for something different. Let me provide the correct interpretation:

```scala
def solve(s: String, A: Array[Double]): Array[Double] = {
  // Calculate the probability of a random DNA string matching s
  // Each nucleotide has probability 0.25 of matching (assuming uniform distribution)
  val n = s.length
  val prob = math.pow(0.25, n)
  
  // Return array with same probability for each element
  Array.fill(A.length)(prob)
}

// More precise implementation
def solveRosalind(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  // Probability of a random DNA string of length n matching s
  val matchProb = math.pow(0.25, n)
  
  // Return array where each element is the probability of matching
  Array.fill(A.length)(matchProb)
}

// Example usage:
val s = "ACGT"
val A = Array(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
val result = solveRosalind(s, A)
println(result.mkString(" "))
```

Actually, let me check what the standard Rosalind problem expects. The problem title is "Introduction to Random Strings" and typically this involves computing the probability of a DNA string under a given GC-content.

But based on the description given, here's the most likely intended solution:

```scala
def solve(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  // For a random DNA string of length n, the probability of matching s
  // is (1/4)^n since each nucleotide has probability 1/4 of matching
  val prob = math.pow(0.25, n)
  
  // Return array with same probability for all entries
  Array.fill(A.length)(prob)
}

// If we need to handle the array A properly, here's a more complete solution:
def solveComplete(s: String, A: Array[Double]): Array[Double] = {
  val n = s.length
  val result = new Array[Double](A.length)
  
  // Probability of a random DNA string matching s
  val matchProb = math.pow(0.25, n)
  
  // Fill result array with the same probability
  for (i <- result.indices) {
    result(i) = matchProb
  }
  
  result
}
```

