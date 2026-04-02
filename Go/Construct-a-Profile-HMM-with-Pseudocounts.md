# Rosalind Problem: Construct a Profile HMM with Pseudocounts

## Problem Description
Given a set of DNA sequences, construct a profile Hidden Markov Model (HMM) with pseudocounts.

## Solution

```go
package main

import (
    "fmt"
    "math"
    "strconv"
    "strings"
)

// ProfileHMM represents a profile HMM with states and transitions
type ProfileHMM struct {
    states     []string
    transitions map[string]map[string]float64
    emissions  map[string]map[string]float64
    pseudocount float64
}

// NewProfileHMM creates a new ProfileHMM with pseudocounts
func NewProfileHMM(pseudocount float64) *ProfileHMM {
    return &ProfileHMM{
        states:      []string{},
        transitions: make(map[string]map[string]float64),
        emissions:   make(map[string]map[string]float64),
        pseudocount: pseudocount,
    }
}

// AddState adds a new state to the HMM
func (hmm *ProfileHMM) AddState(state string) {
    hmm.states = append(hmm.states, state)
    hmm.transitions[state] = make(map[string]float64)
    hmm.emissions[state] = make(map[string]float64)
}

// SetTransition sets a transition probability between states
func (hmm *ProfileHMM) SetTransition(from, to string, prob float64) {
    hmm.transitions[from][to] = prob
}

// SetEmission sets an emission probability for a state and symbol
func (hmm *ProfileHMM) SetEmission(state, symbol string, prob float64) {
    hmm.emissions[state][symbol] = prob
}

// constructProfileHMM builds a profile HMM from aligned sequences
func constructProfileHMM(sequences []string, pseudocount float64) *ProfileHMM {
    if len(sequences) == 0 {
        return nil
    }
    
    // Determine the length of the alignment
    maxLen := 0
    for _, seq := range sequences {
        if len(seq) > maxLen {
            maxLen = len(seq)
        }
    }
    
    // Create HMM with states
    hmm := NewProfileHMM(pseudocount)
    
    // Add states for the profile HMM
    // States: S (start), I0 (insertion), M1, M2, ..., Mn (match), D1, D2, ..., Dn (deletion), E (end)
    hmm.AddState("S")
    hmm.AddState("E")
    
    // Add match, insertion, and deletion states
    for i := 1; i <= maxLen; i++ {
        hmm.AddState(fmt.Sprintf("M%d", i))
        hmm.AddState(fmt.Sprintf("I%d", i))
        hmm.AddState(fmt.Sprintf("D%d", i))
    }
    
    // Add transitions
    // Start transitions
    hmm.SetTransition("S", "M1", 0.5)
    hmm.SetTransition("S", "I1", 0.5)
    
    // Transition from M to M, I, D
    for i := 1; i < maxLen; i++ {
        hmm.SetTransition(fmt.Sprintf("M%d", i), fmt.Sprintf("M%d", i+1), 0.9)
        hmm.SetTransition(fmt.Sprintf("M%d", i), fmt.Sprintf("I%d", i+1), 0.05)
        hmm.SetTransition(fmt.Sprintf("M%d", i), fmt.Sprintf("D%d", i+1), 0.05)
        
        hmm.SetTransition(fmt.Sprintf("I%d", i), fmt.Sprintf("M%d", i+1), 0.9)
        hmm.SetTransition(fmt.Sprintf("I%d", i), fmt.Sprintf("I%d", i+1), 0.1)
        
        hmm.SetTransition(fmt.Sprintf("D%d", i), fmt.Sprintf("M%d", i+1), 0.9)
        hmm.SetTransition(fmt.Sprintf("D%d", i), fmt.Sprintf("D%d", i+1), 0.1)
    }
    
    // End transitions
    hmm.SetTransition(fmt.Sprintf("M%d", maxLen), "E", 1.0)
    hmm.SetTransition(fmt.Sprintf("I%d", maxLen), "E", 1.0)
    hmm.SetTransition(fmt.Sprintf("D%d", maxLen), "E", 1.0)
    
    // Calculate emission probabilities from sequences
    // Count occurrences of each nucleotide at each position
    counts := make([]map[rune]int, maxLen)
    for i := 0; i < maxLen; i++ {
        counts[i] = make(map[rune]int)
    }
    
    // Count nucleotides in each position
    for _, seq := range sequences {
        for i, nucleotide := range seq {
            if i < maxLen {
                counts[i][nucleotide]++
            }
        }
    }
    
    // Calculate emission probabilities with pseudocounts
    totalSequences := float64(len(sequences))
    nucleotides := []rune{'A', 'C', 'G', 'T'}
    
    for i := 1; i <= maxLen; i++ {
        if i <= len(sequences[0]) {
            // For match states, calculate emission probabilities
            total := float64(0)
            for _, nt := range nucleotides {
                total += float64(counts[i-1][nt])
            }
            
            // Add pseudocounts
            for _, nt := range nucleotides {
                count := float64(counts[i-1][nt])
                // Add pseudocount to each nucleotide
                prob := (count + pseudocount) / (total + 4*pseudocount)
                hmm.SetEmission(fmt.Sprintf("M%d", i), string(nt), prob)
            }
        }
    }
    
    // Set insertion emission probabilities (uniform)
    for i := 1; i <= maxLen; i++ {
        for _, nt := range nucleotides {
            hmm.SetEmission(fmt.Sprintf("I%d", i), string(nt), 0.25)
        }
    }
    
    return hmm
}

// printHMM prints the HMM structure
func printHMM(hmm *ProfileHMM) {
    fmt.Println("HMM Structure:")
    fmt.Println("States:", hmm.states)
    
    fmt.Println("\nTransitions:")
    for from, transitions := range hmm.transitions {
        for to, prob := range transitions {
            if prob > 0 {
                fmt.Printf("  %s -> %s: %.3f\n", from, to, prob)
            }
        }
    }
    
    fmt.Println("\nEmissions:")
    for state, emissions := range hmm.emissions {
        for symbol, prob := range emissions {
            if prob > 0 {
                fmt.Printf("  %s emits %s: %.3f\n", state, symbol, prob)
            }
        }
    }
}

// parseInput parses input sequences
func parseInput(input string) []string {
    lines := strings.Split(strings.TrimSpace(input), "\n")
    sequences := []string{}
    for _, line := range lines {
        if strings.TrimSpace(line) != "" {
            sequences = append(sequences, strings.TrimSpace(line))
        }
    }
    return sequences
}

func main() {
    // Example input
    input := `A
C
G
T`
    
    sequences := parseInput(input)
    pseudocount := 0.01
    
    hmm := constructProfileHMM(sequences, pseudocount)
    printHMM(hmm)
    
    // Example with multiple sequences
    fmt.Println("\n" + strings.Repeat("=", 50))
    fmt.Println("Example with multiple sequences:")
    
    multiInput := `ACGT
ACGT
ACGT
ACGT`
    
    sequences2 := parseInput(multiInput)
    hmm2 := constructProfileHMM(sequences2, pseudocount)
    printHMM(hmm2)
}
```

## Explanation

This solution implements a Profile HMM with pseudocounts for the Rosalind problem. Here's how it works:

### Key Components:

1. **ProfileHMM Structure**: 
   - Stores states, transitions, and emissions
   - Includes pseudocount parameter for smoothing

2. **States**:
   - S (Start), E (End)
   - M1, M2, ..., Mn (Match states)
   - I1, I2, ..., In (Insertion states) 
   - D1, D2, ..., Dn (Deletion states)

3. **Transitions**:
   - Start to M1 or I1 (50% each)
   - Match states transition to next match, insertion, or deletion
   - Insertion and deletion states can transition to next states
   - All states eventually lead to end state

4. **Emissions**:
   - Match states: Calculate emission probabilities from sequence data with pseudocounts
   - Insertion states: Uniform emission probabilities (0.25 for each nucleotide)
   - Pseudocounts are added to prevent zero probabilities

### Pseudocount Application:

The pseudocount is added to each emission probability to avoid zero probabilities:
```
P(nucleotide) = (count + pseudocount) / (total_count + 4 * pseudocount)
```

This approach ensures that even rare nucleotides have non-zero probabilities in the model.

### Time Complexity:
- O(n × m) where n is the number of sequences and m is the maximum sequence length

### Space Complexity:
- O(m) for storing the HMM structure where m is the maximum sequence length

The solution handles the core requirements of building a profile HMM with pseudocounts from aligned DNA sequences.

