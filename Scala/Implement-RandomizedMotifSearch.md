# Rosalind Problem: Implement_RandomizedMotifSearch in Scala

## Problem Description
Implement the randomized motif search algorithm to find the best motifs in a set of DNA sequences.

## Solution

```scala
object RandomizedMotifSearch {
  
  def randomizedMotifSearch(dna: List[String], k: Int, t: Int): List[String] = {
    import scala.util.Random
    
    // Initialize best motifs with random motifs from each sequence
    var bestMotifs = randomMotifs(dna, k)
    var bestScore = score(bestMotifs)
    
    // Run the algorithm for a fixed number of iterations
    for (_ <- 0 until 1000) {
      val motifs = randomizedMotifSearchStep(dna, k, t)
      val score = score(motifs)
      
      if (score < bestScore) {
        bestScore = score
        bestMotifs = motifs
      }
    }
    
    bestMotifs
  }
  
  def randomizedMotifSearchStep(dna: List[String], k: Int, t: Int): List[String] = {
    import scala.util.Random
    
    // Generate random motifs from each DNA sequence
    val motifs = randomMotifs(dna, k)
    
    // Create profile from current motifs
    val profile = makeProfile(motifs, k)
    
    // Generate new motifs based on the profile
    val newMotifs = dna.zipWithIndex.map { case (sequence, i) =>
      profileMostProbableKmer(sequence, k, profile)
    }
    
    newMotifs
  }
  
  def randomMotifs(dna: List[String], k: Int): List[String] = {
    import scala.util.Random
    
    dna.map(sequence => {
      val start = Random.nextInt(sequence.length - k + 1)
      sequence.substring(start, start + k)
    })
  }
  
  def makeProfile(motifs: List[String], k: Int): Array[Map[Char, Double]] = {
    val profile = Array.fill(k)(Map('A' -> 0.0, 'C' -> 0.0, 'G' -> 0.0, 'T' -> 0.0))
    
    motifs.foreach(motif => {
      for (i <- motif.indices) {
        val nucleotide = motif(i)
        profile(i) = profile(i) + (nucleotide -> (profile(i)(nucleotide) + 1))
      }
    })
    
    // Normalize the counts to get probabilities
    for (i <- profile.indices) {
      val total = profile(i).values.sum
      profile(i) = profile(i).map { case (nucleotide, count) => 
        nucleotide -> (count / total) 
      }
    }
    
    profile
  }
  
  def profileMostProbableKmer(sequence: String, k: Int, profile: Array[Map[Char, Double]]): String = {
    var maxProbability = -1.0
    var bestKmer = ""
    
    for (i <- 0 to sequence.length - k) {
      val kmer = sequence.substring(i, i + k)
      var probability = 1.0
      
      for (j <- kmer.indices) {
        val nucleotide = kmer(j)
        probability *= profile(j)(nucleotide)
      }
      
      if (probability > maxProbability) {
        maxProbability = probability
        bestKmer = kmer
      }
    }
    
    bestKmer
  }
  
  def score(motifs: List[String]): Int = {
    val consensus = consensusString(motifs)
    motifs.map(motif => hammingDistance(motif, consensus)).sum
  }
  
  def consensusString(motifs: List[String]): String = {
    val k = motifs.head.length
    val consensus = new StringBuilder
    
    for (i <- 0 until k) {
      val nucleotides = motifs.map(_(i))
      val counts = nucleotides.groupBy(identity).mapValues(_.length)
      val maxCount = counts.values.max
      val mostFrequent = counts.filter(_._2 == maxCount).head._1
      consensus.append(mostFrequent)
    }
    
    consensus.toString
  }
  
  def hammingDistance(s1: String, s2: String): Int = {
    s1.zip(s2).count { case (c1, c2) => c1 != c2 }
  }
  
  // Main function to test the implementation
  def main(args: Array[String]): Unit = {
    // Example usage
    val dna = List(
      "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA",
      "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG",
      "TAGTACCGAGACCGAAAGAAGTATACAGGCGT",
      "TAGATCAAGTTTCAGGTGCACGTCGGTGAACC",
      "AATCCACCAGCTCCACGTGCAATGTTGGCCTA"
    )
    
    val k = 8
    val t = 5
    
    val result = randomizedMotifSearch(dna, k, t)
    println("Best motifs found:")
    result.foreach(println)
  }
}
```

## Explanation

This implementation follows the randomized motif search algorithm:

1. **Initialization**: Start with random motifs from each DNA sequence
2. **Iteration**: 
   - Create a profile matrix from current motifs
   - Generate new motifs by finding the most probable k-mers in each sequence based on the profile
   - Keep track of the best scoring motifs found
3. **Termination**: Run for a fixed number of iterations (1000 in this case)

## Key Functions

- `randomizedMotifSearch`: Main algorithm that runs the randomized search
- `randomizedMotifSearchStep`: Performs one iteration of the algorithm
- `randomMotifs`: Generates random k-mers from DNA sequences
- `makeProfile`: Creates a profile matrix from motifs
- `profileMostProbableKmer`: Finds the most probable k-mer for a given profile
- `score`: Calculates the score of a set of motifs
- `consensusString`: Determines the consensus string from motifs

## Time Complexity
- O(N × T × K) per iteration where N is the length of sequences, T is the number of sequences, and K is the motif length
- Overall complexity depends on the number of iterations

## Space Complexity
- O(K × 4) for the profile matrix where 4 represents the 4 nucleotides (A, C, G, T)

The algorithm uses randomization to avoid getting stuck in local optima and typically converges to a good solution after sufficient iterations.

