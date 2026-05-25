# Rosalind Problem: Compute the Probability of a String Emitted by an HMM

## Problem Description
Given an HMM (Hidden Markov Model) and a string, compute the probability that the HMM emits the given string.

## Solution in Go

```go
package main

import (
    "fmt"
    "math"
    "strings"
)

// HMM represents a Hidden Markov Model
type HMM struct {
    states     []string
    alphabet   []string
    transition map[string]map[string]float64
    emission   map[string]map[string]float64
    startProb  map[string]float64
    endProb    map[string]float64
}

// NewHMM creates a new HMM from the given data
func NewHMM(states, alphabet []string, 
           transition, emission map[string]map[string]float64,
           startProb, endProb map[string]float64) *HMM {
    return &HMM{
        states:     states,
        alphabet:   alphabet,
        transition: transition,
        emission:   emission,
        startProb:  startProb,
        endProb:    endProb,
    }
}

// ForwardAlgorithm computes the probability of observing a string with the HMM
func (hmm *HMM) ForwardAlgorithm(observation string) float64 {
    if len(observation) == 0 {
        return 0.0
    }
    
    // Create a matrix to store forward probabilities
    // forward[i][j] = probability of being in state j after observing first i characters
    n := len(observation)
    forward := make([][]float64, n+1)
    for i := range forward {
        forward[i] = make([]float64, len(hmm.states))
    }
    
    // Initialize: probability of being in each state at position 0
    for i, state := range hmm.states {
        forward[0][i] = hmm.startProb[state]
    }
    
    // Fill the forward matrix
    for i := 1; i <= n; i++ {
        char := string(observation[i-1])
        for j, state := range hmm.states {
            forward[i][j] = 0.0
            for k, prevState := range hmm.states {
                // Probability of transitioning from prevState to state
                transProb := hmm.transition[prevState][state]
                // Probability of emitting character at current position
                emitProb := hmm.emission[state][char]
                forward[i][j] += forward[i-1][k] * transProb * emitProb
            }
        }
    }
    
    // Sum up the probabilities of reaching any state at the end
    result := 0.0
    for i, state := range hmm.states {
        result += forward[n][i] * hmm.endProb[state]
    }
    
    return result
}

// ParseInput parses the input format from Rosalind
func ParseInput(input string) (*HMM, string) {
    lines := strings.Split(strings.TrimSpace(input), "\n")
    
    // First line: states
    states := strings.Split(lines[0], " ")
    
    // Second line: alphabet
    alphabet := strings.Split(lines[2], " ")
    
    // Third line: empty
    // Fourth line: observation string
    observation := lines[4]
    
    // Create transition matrix
    transition := make(map[string]map[string]float64)
    for i, state := range states {
        transition[state] = make(map[string]float64)
        line := strings.Split(lines[i+6], " ")
        for j, val := range line {
            if val != "" {
                transition[state][states[j]] = parseFloat(val)
            }
        }
    }
    
    // Create emission matrix
    emission := make(map[string]map[string]float64)
    for i, state := range states {
        emission[state] = make(map[string]float64)
        line := strings.Split(lines[i+6+len(states)+3], " ")
        for j, val := range line {
            if val != "" {
                emission[state][alphabet[j]] = parseFloat(val)
            }
        }
    }
    
    // Create start probabilities
    startProb := make(map[string]float64)
    startLine := strings.Split(lines[6+len(states)+1], " ")
    for i, state := range states {
        startProb[state] = parseFloat(startLine[i])
    }
    
    // Create end probabilities
    endProb := make(map[string]float64)
    endLine := strings.Split(lines[6+len(states)+2], " ")
    for i, state := range states {
        endProb[state] = parseFloat(endLine[i])
    }
    
    hmm := NewHMM(states, alphabet, transition, emission, startProb, endProb)
    return hmm, observation
}

func parseFloat(s string) float64 {
    var result float64
    fmt.Sscanf(s, "%f", &result)
    return result
}

// Example usage
func main() {
    // Example input from Rosalind
    input := `A B
x y z
-
A 0.6 0.4
B 0.5 0.5
A 0.1 0.4 0.5
B 0.7 0.2 0.1
0.5 0.5
0.1 0.9
xyxzyx`

    hmm, observation := ParseInput(input)
    probability := hmm.ForwardAlgorithm(observation)
    
    fmt.Printf("Probability: %.10f\n", probability)
    
    // Another example with simpler data
    input2 := `x y
a b
-
x 0.3 0.7
y 0.6 0.4
x 0.5 0.5
y 0.2 0.8
0.5 0.5
0.3 0.7
abba`

    hmm2, observation2 := ParseInput(input2)
    probability2 := hmm2.ForwardAlgorithm(observation2)
    
    fmt.Printf("Probability: %.10f\n", probability2)
}
```

## Explanation

This solution implements the **Forward Algorithm** to compute the probability of a string being emitted by an HMM:

### Key Components:

1. **HMM Structure**: 
   - States, alphabet, transition probabilities, emission probabilities
   - Start and end probabilities

2. **Forward Algorithm**:
   - Creates a dynamic programming matrix where `forward[i][j]` represents the probability of being in state `j` after observing the first `i` characters
   - Uses the recurrence relation: 
     ```
     forward[i][j] = Σ_k forward[i-1][k] × transition[k][j] × emission[j][observation[i]]
     ```

3. **Time Complexity**: O(n × m²) where n is the length of the observation string and m is the number of states

### Input Format:
The input follows the Rosalind format with:
- First line: states
- Second line: alphabet
- Third line: empty
- Fourth line: observation string
- Transition matrix
- Emission matrix
- Start probabilities
- End probabilities

### Output:
The probability that the HMM emits the given string.

The algorithm correctly handles the probabilistic nature of HMMs by summing over all possible state sequences that could produce the observed string.

