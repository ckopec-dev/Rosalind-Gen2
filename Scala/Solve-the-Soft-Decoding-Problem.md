# Rosalind Problem: Solve_the_Soft_Decoding_Problem

## Problem Description
The soft decoding problem asks us to compute the posterior probabilities of each hidden state at each time step given a sequence of observations and a Hidden Markov Model (HMM).

## Solution in Scala

```scala
import scala.math.log
import scala.math.exp

object SoftDecoding {
  
  def softDecoding(observations: List[Int], 
                   states: List[String], 
                   transition: Map[(String, String), Double], 
                   emission: Map[(String, Int), Double], 
                   initial: Map[String, Double]): List[Map[String, Double]] = {
    
    val n = observations.length
    val m = states.length
    
    // Initialize forward probabilities
    val forward = Array.ofDim[Double](n, m)
    
    // Base case: initial probabilities
    for (i <- states.indices) {
      val state = states(i)
      forward(0)(i) = log(initial(state)) + log(emission((state, observations(0))))
    }
    
    // Forward algorithm
    for (t <- 1 until n) {
      for (j <- states.indices) {
        val stateJ = states(j)
        var sum = Double.NegativeInfinity
        
        for (i <- states.indices) {
          val stateI = states(i)
          val transitionProb = transition((stateI, stateJ))
          val emissionProb = emission((stateJ, observations(t)))
          
          val val1 = forward(t-1)(i)
          val val2 = log(transitionProb)
          val val3 = log(emissionProb)
          
          sum = logAdd(sum, val1 + val2 + val3)
        }
        
        forward(t)(j) = sum
      }
    }
    
    // Compute posterior probabilities
    val result = new Array[Map[String, Double]](n)
    
    for (t <- 0 until n) {
      val posteriors = new scala.collection.mutable.HashMap[String, Double]()
      var total = Double.NegativeInfinity
      
      // Compute unnormalized probabilities
      for (i <- states.indices) {
        val state = states(i)
        posteriors.put(state, forward(t)(i))
        total = logAdd(total, forward(t)(i))
      }
      
      // Normalize to get actual probabilities
      val normalized = posteriors.map { case (state, prob) =>
        (state, exp(prob - total))
      }.toMap
      
      result(t) = normalized
    }
    
    result.toList
  }
  
  // Helper function for log-space addition
  def logAdd(logX: Double, logY: Double): Double = {
    if (logX == Double.NegativeInfinity) logY
    else if (logY == Double.NegativeInfinity) logX
    else {
      val max = math.max(logX, logY)
      max + math.log(math.exp(logX - max) + math.exp(logY - max))
    }
  }
  
  def main(args: Array[String]): Unit = {
    // Example usage
    val observations = List(2, 2, 1, 2, 1, 1, 2, 1, 1, 2)
    val states = List("A", "B")
    val initial = Map("A" -> 0.5, "B" -> 0.5)
    val transition = Map(
      ("A", "A") -> 0.6, ("A", "B") -> 0.4,
      ("B", "A") -> 0.4, ("B", "B") -> 0.6
    )
    val emission = Map(
      ("A", 1) -> 0.2, ("A", 2) -> 0.8,
      ("B", 1) -> 0.5, ("B", 2) -> 0.5
    )
    
    val result = softDecoding(observations, states, transition, emission, initial)
    
    // Print results
    for ((t, posteriors) <- result.zipWithIndex) {
      println(s"Time step $t:")
      for ((state, prob) <- posteriors.toList.sortBy(_._1)) {
        println(s"  $state: ${"%.6f".format(prob)}")
      }
    }
  }
}
```

## Explanation

The solution implements the soft decoding algorithm using the forward algorithm to compute posterior probabilities:

1. **Forward Algorithm**: Computes the probability of being in each state at each time step, given the observations up to that point.

2. **Log-Space Computation**: Uses log-space arithmetic to avoid numerical underflow issues when multiplying small probabilities.

3. **Posterior Probabilities**: Computes the normalized probabilities for each state at each time step.

4. **Helper Function**: `logAdd` handles addition in log-space correctly.

## Key Components

- **Observations**: The sequence of observed symbols
- **States**: The possible hidden states
- **Transition**: Probability of transitioning from one state to another
- **Emission**: Probability of observing a symbol given a state
- **Initial**: Initial state probabilities

## Time Complexity
O(n × m²) where n is the number of observations and m is the number of states.

## Space Complexity
O(n × m) for storing forward probabilities.

This implementation correctly handles the soft decoding problem by computing the posterior probabilities of hidden states at each time step given the observed sequence.

