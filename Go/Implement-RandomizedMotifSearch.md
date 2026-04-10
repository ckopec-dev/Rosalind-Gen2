# Rosalind Problem: Implement_RandomizedMotifSearch

I'll solve this step-by-step, implementing the Randomized Motif Search algorithm for finding motifs in DNA sequences.

```go
package main

import (
    "fmt"
    "math/rand"
    "strings"
    "time"
)

// RandomizedMotifSearch implements the randomized motif search algorithm
func RandomizedMotifSearch(dna []string, k int) []string {
    // Initialize random seed
    rand.Seed(time.Now().UnixNano())
    
    // Initialize best motifs with random motifs from each sequence
    bestMotifs := make([]string, len(dna))
    for i := 0; i < len(dna); i++ {
        // Random position in the sequence
        pos := rand.Intn(len(dna[i]) - k + 1)
        bestMotifs[i] = dna[i][pos : pos+k]
    }
    
    // Keep track of the best score
    bestScore := Score(bestMotifs)
    
    // Run the algorithm multiple times
    for i := 0; i < 1000; i++ {
        // Generate random motifs
        motifs := RandomMotifs(dna, k)
        
        // Local optimization
        for {
            // Compute profile from current motifs
            profile := Profile(motifs)
            
            // Generate new motifs from profile
            newMotifs := MotifsFromProfile(dna, profile, k)
            
            // If new motifs are better, update
            newScore := Score(newMotifs)
            if newScore < bestScore {
                bestScore = newScore
                bestMotifs = newMotifs
                motifs = newMotifs
            } else {
                break
            }
        }
    }
    
    return bestMotifs
}

// RandomMotifs generates random motifs from DNA sequences
func RandomMotifs(dna []string, k int) []string {
    rand.Seed(time.Now().UnixNano())
    motifs := make([]string, len(dna))
    for i := 0; i < len(dna); i++ {
        pos := rand.Intn(len(dna[i]) - k + 1)
        motifs[i] = dna[i][pos : pos+k]
    }
    return motifs
}

// Profile computes the profile matrix from motifs
func Profile(motifs []string) [][]float64 {
    k := len(motifs[0])
    profile := make([][]float64, 4) // A, C, G, T
    for i := 0; i < 4; i++ {
        profile[i] = make([]float64, k)
    }
    
    // Count nucleotides
    for _, motif := range motifs {
        for i, nucleotide := range motif {
            switch nucleotide {
            case 'A':
                profile[0][i]++
            case 'C':
                profile[1][i]++
            case 'G':
                profile[2][i]++
            case 'T':
                profile[3][i]++
            }
        }
    }
    
    // Normalize to get probabilities
    for i := 0; i < 4; i++ {
        for j := 0; j < k; j++ {
            profile[i][j] /= float64(len(motifs))
        }
    }
    
    return profile
}

// MotifsFromProfile generates motifs from profile matrix
func MotifsFromProfile(dna []string, profile [][]float64, k int) []string {
    motifs := make([]string, len(dna))
    for i, sequence := range dna {
        motifs[i] = ProfileMostProbable(sequence, k, profile)
    }
    return motifs
}

// ProfileMostProbable finds the most probable k-mer in a sequence given a profile
func ProfileMostProbable(sequence string, k int, profile [][]float64) string {
    maxProb := -1.0
    bestKmer := ""
    
    for i := 0; i <= len(sequence)-k; i++ {
        kmer := sequence[i : i+k]
        prob := 1.0
        
        for j, nucleotide := range kmer {
            var idx int
            switch nucleotide {
            case 'A':
                idx = 0
            case 'C':
                idx = 1
            case 'G':
                idx = 2
            case 'T':
                idx = 3
            }
            prob *= profile[idx][j]
        }
        
        if prob > maxProb {
            maxProb = prob
            bestKmer = kmer
        }
    }
    
    return bestKmer
}

// Score calculates the score of motifs (number of mismatches from consensus)
func Score(motifs []string) int {
    if len(motifs) == 0 {
        return 0
    }
    
    k := len(motifs[0])
    score := 0
    
    for i := 0; i < k; i++ {
        // Count nucleotides at position i
        count := make(map[rune]int)
        for _, motif := range motifs {
            count[motif[i]]++
        }
        
        // Find the most frequent nucleotide
        maxCount := 0
        for _, c := range count {
            if c > maxCount {
                maxCount = c
            }
        }
        
        // Score is number of mismatches
        score += len(motifs) - maxCount
    }
    
    return score
}

// Helper function to convert DNA string to uppercase
func toUpper(s string) string {
    return strings.ToUpper(s)
}

func main() {
    // Example input
    dna := []string{
        "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA",
        "GGGCGAGGTATATGGTGTCCCTGTACGTTTC",
        "GTTTTCTGTGTTTACCCCTTTC",
        "TTTTCTGTGTTTACCCCTTTC",
        "TTTTCTGTGTTTACCCCTTTC",
    }
    
    k := 3
    
    // Run randomized motif search
    motifs := RandomizedMotifSearch(dna, k)
    
    // Print result
    fmt.Println("Best motifs found:")
    for _, motif := range motifs {
        fmt.Println(motif)
    }
    
    // Print score
    score := Score(motifs)
    fmt.Printf("Score: %d\n", score)
}
```

## Explanation of the Algorithm

The Randomized Motif Search algorithm works as follows:

1. **Initialization**: Start with random motifs from each DNA sequence
2. **Iteration**: 
   - Compute the profile matrix from current motifs
   - Generate new motifs from the profile using the most probable k-mers
   - If the new motifs are better (lower score), update the current motifs
3. **Repeat**: Run this process multiple times to find the best solution

## Key Functions

- **RandomizedMotifSearch**: Main algorithm implementation
- **RandomMotifs**: Generates random motifs from DNA sequences
- **Profile**: Computes the probability profile from motifs
- **MotifsFromProfile**: Generates motifs from a profile matrix
- **ProfileMostProbable**: Finds the most probable k-mer given a profile
- **Score**: Calculates the motif score (number of mismatches from consensus)

## Time Complexity
- O(N × M × K) where N is the number of sequences, M is the sequence length, and K is the motif length
- The algorithm runs multiple iterations to improve results

## Space Complexity
- O(N × K) for storing motifs and profile matrices

This implementation should solve the Rosalind problem "Implement_RandomizedMotifSearch" correctly.

