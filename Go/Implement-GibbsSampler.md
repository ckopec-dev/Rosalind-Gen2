# Rosalind Problem: Implement_GibbsSampler

## Problem Description
Implement the Gibbs sampler for motif finding. Given a set of DNA sequences and a motif length, find the best motifs using Gibbs sampling.

## Solution

```go
package main

import (
    "fmt"
    "math/rand"
    "strings"
    "time"
)

// GibbsSampler implements the Gibbs sampling algorithm for motif finding
func GibbsSampler(dna []string, k, t, N int) []string {
    // Initialize random seed
    rand.Seed(time.Now().UnixNano())
    
    // Initialize random motifs
    motifs := make([]string, t)
    for i := 0; i < t; i++ {
        start := rand.Intn(len(dna[i]) - k + 1)
        motifs[i] = dna[i][start : start+k]
    }
    
    bestMotifs := make([]string, len(motifs))
    copy(bestMotifs, motifs)
    
    for i := 0; i < N; i++ {
        // Choose random motif to remove (1-indexed)
        j := rand.Intn(t)
        
        // Create profile matrix without the j-th motif
        profile := makeProfile(motifs, k, t, j)
        
        // Generate new motif from profile
        newMotif := generateMotif(dna[j], k, profile)
        
        // Update motifs
        motifs[j] = newMotif
        
        // Update best motifs if current is better
        if scoreMotifs(motifs) < scoreMotifs(bestMotifs) {
            copy(bestMotifs, motifs)
        }
    }
    
    return bestMotifs
}

// makeProfile creates a profile matrix from motifs (excluding one)
func makeProfile(motifs []string, k, t, exclude int) [][]float64 {
    profile := make([][]float64, 4) // A, C, G, T
    for i := range profile {
        profile[i] = make([]float64, k)
    }
    
    // Count nucleotides for each position
    for i := 0; i < t; i++ {
        if i == exclude {
            continue
        }
        for j := 0; j < k; j++ {
            nucleotide := motifs[i][j]
            switch nucleotide {
            case 'A':
                profile[0][j]++
            case 'C':
                profile[1][j]++
            case 'G':
                profile[2][j]++
            case 'T':
                profile[3][j]++
            }
        }
    }
    
    // Convert counts to probabilities
    for i := 0; i < 4; i++ {
        for j := 0; j < k; j++ {
            profile[i][j] /= float64(t - 1)
        }
    }
    
    return profile
}

// generateMotif generates a new motif from the profile
func generateMotif(dna string, k int, profile [][]float64) string {
    maxProb := -1.0
    bestKmer := ""
    
    for i := 0; i <= len(dna)-k; i++ {
        kmer := dna[i : i+k]
        prob := 1.0
        
        for j := 0; j < k; j++ {
            nucleotide := kmer[j]
            var p float64
            switch nucleotide {
            case 'A':
                p = profile[0][j]
            case 'C':
                p = profile[1][j]
            case 'G':
                p = profile[2][j]
            case 'T':
                p = profile[3][j]
            }
            prob *= p
        }
        
        if prob > maxProb {
            maxProb = prob
            bestKmer = kmer
        }
    }
    
    return bestKmer
}

// scoreMotifs calculates the score of a set of motifs
func scoreMotifs(motifs []string) int {
    k := len(motifs[0])
    score := 0
    
    for j := 0; j < k; j++ {
        count := make(map[rune]int)
        for i := 0; i < len(motifs); i++ {
            count[rune(motifs[i][j])]++
        }
        
        // Find the maximum count at this position
        maxCount := 0
        for _, c := range count {
            if c > maxCount {
                maxCount = c
            }
        }
        
        // Score is number of sequences minus max count
        score += len(motifs) - maxCount
    }
    
    return score
}

// Helper function to print motifs
func printMotifs(motifs []string) {
    fmt.Println(strings.Join(motifs, "\n"))
}

func main() {
    // Example usage
    dna := []string{
        "CGCCCCTCTCGGGGGTGTTCAGTAAACGGCCA",
        "GGGCGAGGTATGTGTAAGTGATGATGATGAGT",
        "TTGGGCGAGGTATGTGTAAGTGATGATGATGAGT",
        "GAGGCGAGGTATGTGTAAGTGATGATGATGAGT",
        "TTGGGCGAGGTATGTGTAAGTGATGATGATGAGT",
    }
    
    k := 7
    t := 5
    N := 1000
    
    motifs := GibbsSampler(dna, k, t, N)
    fmt.Println("Best motifs found:")
    printMotifs(motifs)
    
    // Calculate and print the score
    score := scoreMotifs(motifs)
    fmt.Printf("Score: %d\n", score)
}
```

## Explanation

This implementation of GibbsSampler follows these steps:

1. **Initialization**: Randomly select k-mers from each DNA sequence to form the initial motifs
2. **Main Loop**: For N iterations:
   - Randomly select one sequence to remove its motif
   - Build a profile matrix from remaining motifs (excluding the selected one)
   - Generate a new motif for the removed sequence using the profile
   - Update the motifs and track the best scoring set of motifs
3. **Profile Construction**: Count nucleotides at each position and convert to probabilities
4. **Motif Generation**: For each sequence, find the k-mer with maximum probability under the profile
5. **Scoring**: Calculate the motif score based on the number of mismatches from the consensus

## Key Components

- **makeProfile**: Creates a probability profile matrix from motifs
- **generateMotif**: Generates a new motif using the profile matrix
- **scoreMotifs**: Calculates the total score of motifs (lower is better)
- **GibbsSampler**: Main algorithm implementation

## Time Complexity
- O(N × t × L × k) where N is the number of iterations, t is the number of sequences, L is the length of sequences, and k is the motif length

## Space Complexity
- O(t × k) for storing motifs and profile matrix

The algorithm uses randomization to escape local optima and find better motif configurations over multiple iterations.

