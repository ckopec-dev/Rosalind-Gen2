# Rosalind Problem: Wright-Fisher's Expected Behavior

## Problem Statement
Given: A positive integer n (n≤1000) and an array A of length 2n containing the numbers 0, 1, 2, ..., 2n-1. Return: The expected number of copies of the allele A[i] in the next generation, where the probability of selecting A[i] is proportional to its current frequency in the population.

## Solution Approach
This is a Wright-Fisher model problem where we need to calculate the expected number of copies of each allele in the next generation. The key insight is that each allele's frequency in the next generation follows a binomial distribution.

## Solution in Scala

```scala
def wrightFisherExpectedBehavior(n: Int, alleles: Array[Int]): Array[Double] = {
  // Calculate the frequency of each allele in the current generation
  val totalAlleles = 2 * n
  val alleleCounts = alleles.groupBy(identity).mapValues(_.length)
  
  // Calculate expected number of copies for each allele
  val expectedCopies = new Array[Double](totalAlleles)
  
  for (i <- 0 until totalAlleles) {
    val frequency = alleleCounts.getOrElse(i, 0).toDouble / totalAlleles
    // Expected copies = 2 * n * frequency (since we have 2n diploid individuals)
    expectedCopies(i) = 2 * n * frequency
  }
  
  expectedCopies
}

// Alternative more concise solution
def wrightFisherExpectedBehaviorConcise(n: Int, alleles: Array[Int]): Array[Double] = {
  val totalAlleles = 2 * n
  val alleleCounts = alleles.groupBy(identity).mapValues(_.length)
  
  (0 until totalAlleles).map { i =>
    val frequency = alleleCounts.getOrElse(i, 0).toDouble / totalAlleles
    2 * n * frequency
  }.toArray
}

// Example usage
val n = 3
val alleles = Array(0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5)
val result = wrightFisherExpectedBehavior(n, alleles)
println(result.mkString(" "))

// For a simpler example with known values
val simpleN = 2
val simpleAlleles = Array(0, 0, 1, 1, 2, 2)
val simpleResult = wrightFisherExpectedBehavior(simpleN, simpleAlleles)
println(simpleResult.mkString(" "))
```

## Explanation

1. **Input Analysis**: We have `n` diploid individuals (so `2n` total alleles) and an array of alleles.

2. **Frequency Calculation**: 
   - Count how many times each allele appears in the current generation
   - Calculate the frequency of each allele (count/total_alleles)

3. **Expected Value Calculation**:
   - In the Wright-Fisher model, each allele's frequency in the next generation follows a binomial distribution
   - Expected copies = 2 × n × frequency (since we have 2n diploid individuals)
   - The factor of 2 accounts for the diploid nature of the organisms

4. **Output**: Return an array where each element represents the expected number of copies of the corresponding allele in the next generation.

## Time and Space Complexity
- **Time Complexity**: O(2n) for counting alleles + O(2n) for expected value calculation = O(n)
- **Space Complexity**: O(2n) for storing allele counts and results

## Test Case Example
For n=2 and alleles=[0,0,1,1,2,2]:
- Total alleles = 6
- Allele 0 appears 2 times → frequency = 2/6 = 1/3
- Allele 1 appears 2 times → frequency = 2/6 = 1/3  
- Allele 2 appears 2 times → frequency = 2/6 = 1/3
- Expected copies = 2 × 2 × (1/3) = 4/3 ≈ 1.33

The result would be [1.33, 1.33, 1.33] for alleles 0, 1, and 2 respectively.

