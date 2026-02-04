# Rosalind Problem: Estimate the Parameters of an HMM

## Problem Understanding

This problem asks us to estimate the parameters of a Hidden Markov Model (HMM) given:
- A sequence of observed symbols
- The number of hidden states
- The transition probabilities between hidden states
- The emission probabilities for each state

## Solution Approach

We'll implement the Baum-Welch algorithm to estimate the HMM parameters through iterative Expectation-Maximization.

```go
package main

import (
    "fmt"
    "math"
    "strings"
)

// HMM represents a Hidden Markov Model
type HMM struct {
    states     int
    observations []string
    transition [][]float64
    emission   [][]float64
    pi         []float64
}

// forwardAlgorithm computes the forward probabilities
func (hmm *HMM) forward(observations []string) [][]float64 {
    n := len(observations)
    alpha := make([][]float64, n)
    for i := range alpha {
        alpha[i] = make([]float64, hmm.states)
    }
    
    // Initialize
    for i := 0; i < hmm.states; i++ {
        obsIdx := hmm.getObservationIndex(observations[0])
        alpha[0][i] = hmm.pi[i] * hmm.emission[i][obsIdx]
    }
    
    // Forward algorithm
    for t := 1; t < n; t++ {
        obsIdx := hmm.getObservationIndex(observations[t])
        for j := 0; j < hmm.states; j++ {
            alpha[t][j] = 0
            for i := 0; i < hmm.states; i++ {
                alpha[t][j] += alpha[t-1][i] * hmm.transition[i][j]
            }
            alpha[t][j] *= hmm.emission[j][obsIdx]
        }
    }
    
    return alpha
}

// backwardAlgorithm computes the backward probabilities
func (hmm *HMM) backward(observations []string) [][]float64 {
    n := len(observations)
    beta := make([][]float64, n)
    for i := range beta {
        beta[i] = make([]float64, hmm.states)
    }
    
    // Initialize
    for i := 0; i < hmm.states; i++ {
        beta[n-1][i] = 1
    }
    
    // Backward algorithm
    for t := n - 2; t >= 0; t-- {
        obsIdx := hmm.getObservationIndex(observations[t+1])
        for i := 0; i < hmm.states; i++ {
            beta[t][i] = 0
            for j := 0; j < hmm.states; j++ {
                beta[t][i] += hmm.transition[i][j] * hmm.emission[j][obsIdx] * beta[t+1][j]
            }
        }
    }
    
    return beta
}

// getObservationIndex returns the index of an observation
func (hmm *HMM) getObservationIndex(obs string) int {
    for i, o := range hmm.observations {
        if o == obs {
            return i
        }
    }
    return -1
}

// estimateParameters estimates HMM parameters using Baum-Welch
func (hmm *HMM) estimateParameters(observations []string, maxIterations int) {
    n := len(observations)
    
    for iter := 0; iter < maxIterations; iter++ {
        // Compute forward and backward probabilities
        alpha := hmm.forward(observations)
        beta := hmm.backward(observations)
        
        // Compute gamma (state probabilities)
        gamma := make([][]float64, n)
        for t := 0; t < n; t++ {
            gamma[t] = make([]float64, hmm.states)
            sum := 0.0
            for i := 0; i < hmm.states; i++ {
                gamma[t][i] = alpha[t][i] * beta[t][i]
                sum += gamma[t][i]
            }
            // Normalize
            for i := 0; i < hmm.states; i++ {
                gamma[t][i] /= sum
            }
        }
        
        // Compute xi (transition probabilities)
        xi := make([][][]float64, n-1)
        for t := 0; t < n-1; t++ {
            xi[t] = make([][]float64, hmm.states)
            for i := 0; i < hmm.states; i++ {
                xi[t][i] = make([]float64, hmm.states)
            }
        }
        
        for t := 0; t < n-1; t++ {
            obsIdx := hmm.getObservationIndex(observations[t+1])
            sum := 0.0
            for i := 0; i < hmm.states; i++ {
                for j := 0; j < hmm.states; j++ {
                    xi[t][i][j] = alpha[t][i] * hmm.transition[i][j] * hmm.emission[j][obsIdx] * beta[t+1][j]
                    sum += xi[t][i][j]
                }
            }
            // Normalize
            for i := 0; i < hmm.states; i++ {
                for j := 0; j < hmm.states; j++ {
                    xi[t][i][j] /= sum
                }
            }
        }
        
        // Update pi
        for i := 0; i < hmm.states; i++ {
            hmm.pi[i] = gamma[0][i]
        }
        
        // Update transition probabilities
        for i := 0; i < hmm.states; i++ {
            for j := 0; j < hmm.states; j++ {
                numerator := 0.0
                denominator := 0.0
                for t := 0; t < n-1; t++ {
                    numerator += xi[t][i][j]
                    denominator += gamma[t][i]
                }
                if denominator > 0 {
                    hmm.transition[i][j] = numerator / denominator
                } else {
                    hmm.transition[i][j] = 0
                }
            }
        }
        
        // Update emission probabilities
        for i := 0; i < hmm.states; i++ {
            for k := 0; k < len(hmm.observations); k++ {
                numerator := 0.0
                denominator := 0.0
                for t := 0; t < n; t++ {
                    if hmm.getObservationIndex(observations[t]) == k {
                        numerator += gamma[t][i]
                    }
                    denominator += gamma[t][i]
                }
                if denominator > 0 {
                    hmm.emission[i][k] = numerator / denominator
                } else {
                    hmm.emission[i][k] = 0
                }
            }
        }
    }
}

// printHMM prints the HMM parameters in the required format
func (hmm *HMM) printHMM() {
    fmt.Println("Initial probabilities:")
    fmt.Printf("pi: %v\n", hmm.pi)
    
    fmt.Println("Transition probabilities:")
    for i, row := range hmm.transition {
        fmt.Printf("State %d: %v\n", i, row)
    }
    
    fmt.Println("Emission probabilities:")
    for i, row := range hmm.emission {
        fmt.Printf("State %d: %v\n", i, row)
    }
}

// parseInput parses the input data
func parseInput(input string) (*HMM, []string) {
    lines := strings.Split(strings.TrimSpace(input), "\n")
    
    // First line: number of states
    states := 0
    fmt.Sscanf(lines[0], "%d", &states)
    
    // Second line: observations
    obsLine := strings.Split(lines[1], " ")
    observations := make([]string, len(obsLine))
    for i, obs := range obsLine {
        observations[i] = strings.TrimSpace(obs)
    }
    
    // Third line: sequence of observations
    sequence := strings.Split(strings.TrimSpace(lines[3]), " ")
    
    // Initialize HMM with uniform probabilities
    hmm := &HMM{
        states:       states,
        observations: observations,
        transition:   make([][]float64, states),
        emission:     make([][]float64, states),
        pi:           make([]float64, states),
    }
    
    // Initialize with uniform probabilities
    for i := range hmm.transition {
        hmm.transition[i] = make([]float64, states)
        for j := range hmm.transition[i] {
            hmm.transition[i][j] = 1.0 / float64(states)
        }
    }
    
    for i := range hmm.emission {
        hmm.emission[i] = make([]float64, len(observations))
        for j := range hmm.emission[i] {
            hmm.emission[i][j] = 1.0 / float64(len(observations))
        }
    }
    
    for i := range hmm.pi {
        hmm.pi[i] = 1.0 / float64(states)
    }
    
    return hmm, sequence
}

func main() {
    // Example input (this would come from the actual Rosalind dataset)
    input := `3
A B C
--------
A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C A B C