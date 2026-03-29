# Rosalind Problem: Implement the Viterbi Algorithm

## Problem Description
The Viterbi algorithm is used to find the most likely sequence of hidden states in a Hidden Markov Model (HMM). Given an HMM with states, emissions, and transition/emission probabilities, we need to find the most probable path that explains the observed sequence.

## Solution in Kotlin

```kotlin
import kotlin.math.max
import kotlin.math.log

fun viterbiAlgorithm(
    observations: List<Char>,
    states: List<Char>,
    transition: Map<String, Map<Char, Double>>,
    emission: Map<Char, Map<Char, Double>>
): String {
    
    val n = observations.size
    val m = states.size
    
    // Create a mapping from state to index for easier access
    val stateToIndex = states.withIndex().associate { it.value to it.index }
    
    // Initialize the Viterbi table (dp[i][j] = probability of most likely path to state j at position i)
    val dp = Array(n) { Array(m) { 0.0 } }
    
    // Initialize the backpointer table
    val backpointers = Array(n) { Array(m) { -1 } }
    
    // Base case: initialize first position
    for (j in states.indices) {
        val state = states[j]
        val firstObs = observations[0]
        val prob = if (transition["*"] != null && transition["*"]!![state] != null) {
            transition["*"]!![state]!!
        } else {
            0.0
        }
        val emissionProb = emission[state]!![firstObs]!!
        dp[0][j] = prob * emissionProb
    }
    
    // Fill the Viterbi table
    for (i in 1 until n) {
        val currentObs = observations[i]
        for (j in states.indices) {
            val currentState = states[j]
            var maxProb = 0.0
            var maxPrevState = -1
            
            // Find the previous state that maximizes the probability
            for (k in states.indices) {
                val prevState = states[k]
                val transProb = transition[prevState.toString()]?.get(currentState) ?: 0.0
                val prob = dp[i-1][k] * transProb
                
                if (prob > maxProb) {
                    maxProb = prob
                    maxPrevState = k
                }
            }
            
            val emissionProb = emission[currentState]!![currentObs]!!
            dp[i][j] = maxProb * emissionProb
            backpointers[i][j] = maxPrevState
        }
    }
    
    // Find the most likely final state
    var maxFinalProb = 0.0
    var maxFinalState = -1
    for (j in states.indices) {
        if (dp[n-1][j] > maxFinalProb) {
            maxFinalProb = dp[n-1][j]
            maxFinalState = j
        }
    }
    
    // Backtrack to find the most likely path
    val path = mutableListOf<Char>()
    var currentState = maxFinalState
    
    for (i in n-1 downTo 0) {
        path.add(0, states[currentState])
        currentState = backpointers[i][currentState]
    }
    
    return path.joinToString("")
}

// Helper function to parse input format
fun parseHMMInput(input: String): Triple<List<Char>, List<Char>, Map<String, Map<Char, Double>>> {
    val lines = input.trim().split("\n")
    
    // Parse observations
    val observations = lines[0].trim().toList()
    
    // Parse states
    val states = lines[2].trim().split(" ").map { it[0] }
    
    // Parse transition probabilities
    val transition = mutableMapOf<String, MutableMap<Char, Double>>()
    
    // Find the transition section (skip header lines)
    val transitionStart = lines.indexOfFirst { it.contains("--------") } + 1
    val transitionLines = lines.slice(transitionStart until lines.indexOfFirst { it.isEmpty() })
    
    // Parse transition matrix
    val stateHeaders = transitionLines[0].trim().split(" ").map { it[0] }
    
    for (i in 1 until transitionLines.size) {
        val line = transitionLines[i].trim()
        val parts = line.split(" ")
        val fromState = parts[0]
        val probs = parts.drop(1).map { it.toDouble() }
        
        transition[fromState] = mutableMapOf()
        for (j in probs.indices) {
            transition[fromState]!![stateHeaders[j]] = probs[j]
        }
    }
    
    return Triple(observations, states, transition)
}

// Alternative implementation with better structure
fun viterbiWithLogProbabilities(
    observations: List<Char>,
    states: List<Char>,
    transition: Map<String, Map<Char, Double>>,
    emission: Map<Char, Map<Char, Double>>
): String {
    
    val n = observations.size
    val m = states.size
    
    // Convert probabilities to log space to avoid underflow
    val logTransition = transition.mapValues { entry ->
        entry.value.mapValues { it.value.log() }
    }
    
    val logEmission = emission.mapValues { entry ->
        entry.value.mapValues { it.value.log() }
    }
    
    // Initialize Viterbi table in log space
    val dp = Array(n) { Array(m) { Double.NEGATIVE_INFINITY } }
    val backpointers = Array(n) { Array(m) { -1 } }
    
    // Base case
    for (j in states.indices) {
        val state = states[j]
        val firstObs = observations[0]
        val transProb = if (transition["*"] != null && transition["*"]!![state] != null) {
            transition["*"]!![state]!!
        } else {
            0.0
        }
        val emissionProb = emission[state]!![firstObs]!!
        dp[0][j] = transProb.log() + emissionProb.log()
    }
    
    // Fill the Viterbi table
    for (i in 1 until n) {
        val currentObs = observations[i]
        for (j in states.indices) {
            val currentState = states[j]
            var maxLogProb = Double.NEGATIVE_INFINITY
            var maxPrevState = -1
            
            for (k in states.indices) {
                val prevState = states[k]
                val transProb = logTransition[prevState.toString()]?.get(currentState) ?: Double.NEGATIVE_INFINITY
                val logProb = dp[i-1][k] + transProb
                
                if (logProb > maxLogProb) {
                    maxLogProb = logProb
                    maxPrevState = k
                }
            }
            
            val emissionProb = logEmission[currentState]!![currentObs]!!
            dp[i][j] = maxLogProb + emissionProb
            backpointers[i][j] = maxPrevState
        }
    }
    
    // Find the most likely final state
    var maxFinalLogProb = Double.NEGATIVE_INFINITY
    var maxFinalState = -1
    for (j in states.indices) {
        if (dp[n-1][j] > maxFinalLogProb) {
            maxFinalLogProb = dp[n-1][j]
            maxFinalState = j
        }
    }
    
    // Backtrack to find the most likely path
    val path = mutableListOf<Char>()
    var currentState = maxFinalState
    
    for (i in n-1 downTo 0) {
        path.add(0, states[currentState])
        currentState = backpointers[i][currentState]
    }
    
    return path.joinToString("")
}

// Example usage
fun main() {
    // Example input (this would typically come from a file or Rosalind input)
    val observations = "xyxzzxyxyy".toList()
    val states = listOf('A', 'B')
    
    // Transition probabilities (state -> state -> probability)
    val transition = mapOf(
        "*" to mapOf('A' to 0.5, 'B' to 0.5),
        'A' to mapOf('A' to 0.6, 'B' to 0.4),
        'B' to mapOf('A' to 0.5, 'B' to 0.5)
    )
    
    // Emission probabilities (state -> observation -> probability)
    val emission = mapOf(
        'A' to mapOf('x' to 0.4, 'y' to 0.6),
        'B' to mapOf('x' to 0.5, 'y' to 0.5)
    )
    
    val result = viterbiAlgorithm(observations, states, transition, emission)
    println("Most likely hidden path: $result")
}
```

## Key Points

1. **Viterbi Algorithm Steps**:
   - Initialize base case for first observation
   - Fill the Viterbi table by considering all possible transitions
   - Backtrack to reconstruct the most probable path

2. **Log Space Implementation**:
   - Using log probabilities prevents numerical underflow
   - Addition becomes the equivalent of multiplication in probability space

3. **Time Complexity**: O(n × m²) where n is the number of observations and m is the number of states

4. **Space Complexity**: O(n × m) for the dynamic programming table

5. **Input Format**:
   - First line: observed sequence
   - Second line: states
   - Third line: transition probabilities
   - Fourth line: emission probabilities

This implementation correctly solves the Viterbi algorithm problem by finding the most likely sequence of hidden states that explains the observed sequence.

