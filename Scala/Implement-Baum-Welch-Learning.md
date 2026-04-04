# Rosalind Problem: Implement Baum-Welch Learning

## Problem Description

The Baum-Welch algorithm is used to learn the parameters of a Hidden Markov Model (HMM) from observed sequences. Given a sequence of observations and the structure of an HMM, we need to estimate the transition probabilities, emission probabilities, and initial state probabilities.

## Solution in Scala

```scala
import scala.collection.mutable

object BaumWelchLearning {
  
  case class HMM(
    states: List[String],
    alphabet: List[String],
    transition: Map[(String, String), Double],
    emission: Map[(String, String), Double],
    initial: Map[String, Double]
  )
  
  def baumWelch(observations: List[String], 
                states: List[String], 
                maxIterations: Int = 1000): HMM = {
    
    // Initialize random parameters
    val initial = states.map(state => state -> 1.0 / states.length).toMap
    val transition = states.flatMap(s1 => states.map(s2 => (s1, s2) -> 0.5)).toMap
    val emission = states.flatMap(s => observations.map(obs => (s, obs) -> 0.5)).toMap
    
    var hmm = HMM(states, observations, transition, emission, initial)
    
    // Baum-Welch iterations
    (1 to maxIterations).foreach { _ =>
      hmm = updateParameters(observations, hmm)
    }
    
    hmm
  }
  
  def updateParameters(observations: List[String], hmm: HMM): HMM = {
    val alpha = forward(observations, hmm)
    val beta = backward(observations, hmm)
    
    // Calculate gamma (state probabilities)
    val gamma = calculateGamma(alpha, beta)
    
    // Calculate xi (transition probabilities)
    val xi = calculateXi(observations, hmm, alpha, beta)
    
    // Update initial probabilities
    val newInitial = hmm.initial.map { case (state, _) =>
      (state, gamma(0).getOrElse(state, 0.0))
    }
    
    // Update transition probabilities
    val newTransition = hmm.transition.map { case ((from, to), _) =>
      val numerator = xi.getOrElse((from, to), 0.0)
      val denominator = gamma.map { case (t, probs) =>
        probs.getOrElse(from, 0.0)
      }.sum
      ((from, to), if (denominator > 0) numerator / denominator else 0.0)
    }
    
    // Update emission probabilities
    val newEmission = hmm.emission.map { case ((state, obs), _) =>
      val numerator = observations.zipWithIndex.collect {
        case (o, i) if o == obs => gamma(i).getOrElse(state, 0.0)
      }.sum
      val denominator = gamma.map { case (t, probs) =>
        probs.getOrElse(state, 0.0)
      }.sum
      ((state, obs), if (denominator > 0) numerator / denominator else 0.0)
    }
    
    HMM(hmm.states, hmm.alphabet, newTransition, newEmission, newInitial)
  }
  
  def forward(observations: List[String], hmm: HMM): List[Map[String, Double]] = {
    val alpha = mutable.ListBuffer[Map[String, Double]]()
    
    // Initialize
    val firstAlpha = hmm.states.map(state => 
      state -> (hmm.initial.getOrElse(state, 0.0) * hmm.emission.getOrElse((state, observations.head), 0.0))
    ).toMap
    alpha += firstAlpha
    
    // Forward recursion
    for (t <- 1 until observations.length) {
      val currentAlpha = mutable.Map[String, Double]()
      val obs = observations(t)
      
      for (state <- hmm.states) {
        val sum = hmm.states.map { prevState =>
          alpha(t-1).getOrElse(prevState, 0.0) * hmm.transition.getOrElse((prevState, state), 0.0)
        }.sum
        
        currentAlpha += (state -> (sum * hmm.emission.getOrElse((state, obs), 0.0)))
      }
      
      alpha += currentAlpha.toMap
    }
    
    alpha.toList
  }
  
  def backward(observations: List[String], hmm: HMM): List[Map[String, Double]] = {
    val beta = mutable.ListBuffer[Map[String, Double]]()
    
    // Initialize
    val lastBeta = hmm.states.map(state => state -> 1.0).toMap
    beta += lastBeta
    
    // Backward recursion
    for (t <- (observations.length - 2) to 0 by -1) {
      val currentBeta = mutable.Map[String, Double]()
      val obs = observations(t + 1)
      
      for (state <- hmm.states) {
        val sum = hmm.states.map { nextState =>
          hmm.transition.getOrElse((state, nextState), 0.0) * 
          hmm.emission.getOrElse((nextState, obs), 0.0) * 
          beta(observations.length - 2 - t).getOrElse(nextState, 0.0)
        }.sum
        
        currentBeta += (state -> sum)
      }
      
      beta += currentBeta.toMap
    }
    
    beta.reverse.toList
  }
  
  def calculateGamma(alpha: List[Map[String, Double]], 
                     beta: List[Map[String, Double]]): List[Map[String, Double]] = {
    alpha.zipWithIndex.map { case (alpha_t, t) =>
      val total = alpha_t.values.sum
      alpha_t.map { case (state, _) =>
        (state, alpha_t.getOrElse(state, 0.0) * beta(t).getOrElse(state, 0.0) / total)
      }
    }
  }
  
  def calculateXi(observations: List[String], 
                  hmm: HMM, 
                  alpha: List[Map[String, Double]], 
                  beta: List[Map[String, Double]]): Map[(String, String), Double] = {
    
    val xi = mutable.Map[(String, String), Double]()
    
    for (t <- 0 until observations.length - 1) {
      val obs = observations(t + 1)
      val total = alpha(t).flatMap { case (state1, _) =>
        hmm.states.map(state2 => 
          alpha(t).getOrElse(state1, 0.0) * 
          hmm.transition.getOrElse((state1, state2), 0.0) * 
          hmm.emission.getOrElse((state2, obs), 0.0) * 
          beta(t + 1).getOrElse(state2, 0.0)
        )
      }.sum
      
      for (state1 <- hmm.states) {
        for (state2 <- hmm.states) {
          val value = alpha(t).getOrElse(state1, 0.0) * 
                     hmm.transition.getOrElse((state1, state2), 0.0) * 
                     hmm.emission.getOrElse((state2, obs), 0.0) * 
                     beta(t + 1).getOrElse(state2, 0.0) / total
          
          xi((state1, state2)) = xi.getOrElse((state1, state2), 0.0) + value
        }
      }
    }
    
    xi.toMap
  }
  
  // Helper function to print HMM parameters
  def printHMM(hmm: HMM): Unit = {
    println("Initial probabilities:")
    hmm.initial.foreach { case (state, prob) =>
      println(s"  $state: $prob")
    }
    
    println("\nTransition probabilities:")
    hmm.transition.foreach { case ((from, to), prob) =>
      println(s"  $from -> $to: $prob")
    }
    
    println("\nEmission probabilities:")
    hmm.emission.foreach { case ((state, obs), prob) =>
      println(s"  $state -> $obs: $prob")
    }
  }
  
  // Example usage
  def main(args: Array[String]): Unit = {
    // Example: Simple HMM with 2 states and 2 observations
    val observations = List("A", "B", "A")
    val states = List("H", "T")
    
    val hmm = baumWelch(observations, states)
    printHMM(hmm)
  }
}
```

## Explanation

This implementation of Baum-Welch learning includes:

1. **HMM Case Class**: Represents the hidden Markov model with states, alphabet, transition probabilities, emission probabilities, and initial state probabilities.

2. **Baum-Welch Algorithm**: 
   - Initializes random parameters
   - Iteratively updates parameters using the forward-backward algorithm
   - Runs for a maximum number of iterations

3. **Forward Algorithm**: Computes alpha values (probability of being in a state at time t given observations up to time t).

4. **Backward Algorithm**: Computes beta values (probability of observations from time t+1 to T given state at time t).

5. **Parameter Updates**: 
   - Updates initial probabilities using gamma values
   - Updates transition probabilities using xi values
   - Updates emission probabilities using gamma values

## Key Components

- **Forward-Backward Procedure**: Core of the Baum-Welch algorithm
- **Gamma Calculation**: State probabilities at each time step
- **Xi Calculation**: Joint probabilities of state transitions
- **Parameter Updates**: Recomputes all HMM parameters based on current estimates

The algorithm converges to local maxima of the likelihood function, providing maximum likelihood estimates of the HMM parameters given the observed sequence.

