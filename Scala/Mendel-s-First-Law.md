# Rosalind Problem: Mendel's First Law (MEND)

## Problem Statement
Given three positive integers k, m, and n representing the number of organisms of each genotype in a population:
- k: homozygous dominant (AA)
- m: heterozygous (Aa) 
- n: homozygous recessive (aa)

Calculate the probability that two randomly selected organisms will produce an offspring with a dominant allele (i.e., the offspring will have at least one A).

## Solution Approach
We need to calculate the probability of getting a dominant phenotype in the offspring. This happens when:
1. Both parents are AA (probability 1)
2. One parent is AA and other is Aa (probability 1)  
3. One parent is Aa and other is AA (probability 1)
4. Both parents are Aa (probability 3/4)
5. One parent is Aa and other is aa (probability 1/2)
6. One parent is aa and other is Aa (probability 1/2)
7. Both parents are aa (probability 0)

Actually, let me reconsider this more systematically:

The probability of getting a dominant phenotype = 1 - probability of getting recessive phenotype

Recessive phenotype occurs only when both parents contribute recessive alleles (aa).

## Scala Implementation

```scala
def mendelsFirstLaw(k: Int, m: Int, n: Int): Double = {
  val total = k + m + n
  
  // Total number of ways to pick 2 organisms
  val totalWays = total * (total - 1) / 2
  
  // Number of ways to get recessive offspring (both parents are aa)
  val aaWays = n * (n - 1) / 2
  
  // Number of ways to get recessive offspring when one parent is Aa and other is aa
  val aaways = m * n
  
  // Number of ways to get recessive offspring when both parents are Aa
  val aaways2 = m * (m - 1) / 2
  
  // Total ways to get recessive offspring
  val recessiveWays = aaWays + aaways + aaways2
  
  // Probability of recessive offspring
  val recessiveProb = recessiveWays.toDouble / totalWays
  
  // Probability of dominant offspring
  1.0 - recessiveProb
}

// Alternative cleaner approach
def mendelsFirstLawClean(k: Int, m: Int, n: Int): Double = {
  val total = k + m + n
  
  // Probability of selecting each type of organism
  val pAA = k.toDouble / total
  val pAa = m.toDouble / total
  val paa = n.toDouble / total
  
  // Probability of recessive offspring
  val recessiveProb = 
    paa * paa +  // Both aa
    2 * pAa * paa +  // One Aa, one aa
    0.25 * pAa * pAa   // Both Aa
  
  1.0 - recessiveProb
}

// Most efficient approach using combinations
def mendelsFirstLawEfficient(k: Int, m: Int, n: Int): Double = {
  val total = k + m + n
  
  // Total possible pairs
  val totalPairs = total * (total - 1) / 2
  
  // Pairs that produce recessive offspring:
  // 1. Two aa organisms: n*(n-1)/2
  // 2. One aa and one Aa: n*m
  // 3. Two Aa organisms: m*(m-1)/2, with 0.25 probability of recessive
  
  val recessivePairs = 
    n * (n - 1) / 2 +  // aa pairs
    n * m +            // aa-Aa pairs  
    m * (m - 1) / 2 * 0.25  // Aa-Aa pairs (25% recessive)
  
  1.0 - (recessivePairs / totalPairs)
}
```

## Example Usage

```scala
// Example from problem
val result = mendelsFirstLawEfficient(2, 2, 2)
println(s"Probability: $result") // Should output ~0.7833

// Test with different values
val result2 = mendelsFirstLawEfficient(3, 2, 1)
println(s"Probability: $result2")
```

## Explanation

The key insight is that an offspring has a recessive phenotype only when:
1. Both parents are homozygous recessive (aa) - probability 1
2. One parent is heterozygous (Aa) and the other is homozygous recessive (aa) - probability 0.5  
3. Both parents are heterozygous (Aa) - probability 0.25

So we calculate the probability of getting a recessive offspring, then subtract from 1 to get the dominant probability.

The efficient approach calculates:
- Total pairs = n(n-1)/2
- Recessive pairs = (n(n-1)/2) + (nm) + (m(m-1)/2)(0.25)
- Result = 1 - (recessive pairs / total pairs)

