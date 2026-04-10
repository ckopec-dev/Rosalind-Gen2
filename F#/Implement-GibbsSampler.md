# Rosalind Problem: Implement_GibbsSampler in F#

## Problem Description
Implement the Gibbs sampler algorithm for motif finding. The Gibbs sampler is a probabilistic algorithm that finds motifs in a set of DNA sequences.

## Solution

```fsharp
open System
open System.Collections.Generic

// Helper function to calculate nucleotide probabilities
let calculateProbabilities (sequence: string) (k: int) =
    let probs = Map.empty
    let mutable probs = Map.empty
    for i in 0..(sequence.Length - k) do
        let kmer = sequence.Substring(i, k)
        probs <- 
            if Map.containsKey kmer probs then
                probs |> Map.add kmer (probs.[kmer] + 1.0)
            else
                probs |> Map.add kmer 1.0
    probs

// Helper function to generate random index from probabilities
let weightedRandomIndex (probabilities: Map<string, float>) =
    let total = probabilities.Values |> Seq.sum
    let rand = Random()
    let target = rand.NextDouble() * total
    let mutable cumulative = 0.0
    let mutable result = 0
    let mutable i = 0
    for (KeyValue(kmer, prob)) in probabilities do
        cumulative <- cumulative + prob
        if cumulative >= target then
            result <- i
            break
        i <- i + 1
    result

// Helper function to get k-mer at position
let getKmer (sequence: string) (position: int) (k: int) =
    sequence.Substring(position, k)

// Helper function to calculate motif score
let calculateMotifScore (motifs: string[]) =
    let k = motifs.[0].Length
    let score = ref 0.0
    for i in 0..(k - 1) do
        let nucleotides = motifs |> Array.map (fun motif -> motif.[i])
        let counts = 
            nucleotides 
            |> Seq.groupBy id 
            |> Seq.map (fun (nucleotide, group) -> (nucleotide, Seq.length group))
            |> Seq.toList
        let maxCount = counts |> List.maxBy snd |> snd
        *score <- !score + float (k - maxCount)
    !score

// Main GibbsSampler implementation
let gibbsSampler (dna: string[]) (k: int) (t: int) (n: int) =
    let random = Random()
    
    // Initialize random motifs
    let motifs = Array.zeroCreate t
    for i in 0..(t - 1) do
        let sequenceLength = dna.[i].Length
        let position = random.Next(0, sequenceLength - k + 1)
        motifs.[i] <- dna.[i].Substring(position, k)
    
    let bestMotifs = Array.copy motifs
    let mutable bestScore = calculateMotifScore motifs
    
    for iteration in 0..(n - 1) do
        // Choose random sequence to remove
        let randomSequenceIndex = random.Next(0, t)
        
        // Create profile from remaining motifs
        let profile = Array.zeroCreate k
        for i in 0..(k - 1) do
            profile.[i] <- Map.empty
            
        // Build profile from all motifs except the one being removed
        for i in 0..(t - 1) do
            if i <> randomSequenceIndex then
                let motif = motifs.[i]
                for j in 0..(k - 1) do
                    let nucleotide = motif.[j]
                    let currentProfile = profile.[j]
                    let newProfile = 
                        if Map.containsKey nucleotide currentProfile then
                            currentProfile |> Map.add nucleotide (currentProfile.[nucleotide] + 1.0)
                        else
                            currentProfile |> Map.add nucleotide 1.0
                    profile.[j] <- newProfile
        
        // Normalize profile
        for i in 0..(k - 1) do
            let total = profile.[i].Values |> Seq.sum
            let normalizedProfile = 
                profile.[i]
                |> Map.map (fun _ value -> value / total)
            profile.[i] <- normalizedProfile
        
        // Generate new motif for the removed sequence
        let sequence = dna.[randomSequenceIndex]
        let probabilities = Array.zeroCreate (sequence.Length - k + 1)
        for i in 0..(sequence.Length - k) do
            let kmer = sequence.Substring(i, k)
            let prob = ref 1.0
            for j in 0..(k - 1) do
                let nucleotide = kmer.[j]
                let profileForPosition = profile.[j]
                if Map.containsKey nucleotide profileForPosition then
                    *prob <- !prob * profileForPosition.[nucleotide]
                else
                    *prob <- !prob * 0.0
            probabilities.[i] <- !prob
        
        // Choose new position based on probabilities
        let maxProb = probabilities |> Array.max
        if maxProb > 0.0 then
            let cumulative = Array.zeroCreate (sequence.Length - k + 1)
            cumulative.[0] <- probabilities.[0]
            for i in 1..(sequence.Length - k) do
                cumulative.[i] <- cumulative.[i - 1] + probabilities.[i]
            
            let total = cumulative.[sequence.Length - k]
            let rand = random.NextDouble() * total
            let mutable newMotifIndex = 0
            for i in 0..(sequence.Length - k) do
                if cumulative.[i] >= rand then
                    newMotifIndex <- i
                    break
            motifs.[randomSequenceIndex] <- sequence.Substring(newMotifIndex, k)
        else
            // If all probabilities are zero, choose randomly
            let position = random.Next(0, sequence.Length - k + 1)
            motifs.[randomSequenceIndex] <- sequence.Substring(position, k)
        
        // Update best motifs if current ones are better
        let currentScore = calculateMotifScore motifs
        if currentScore < bestScore then
            bestScore <- currentScore
            Array.blit motifs 0 bestMotifs 0 t
    
    bestMotifs

// Alternative cleaner implementation
let gibbsSamplerClean (dna: string[]) (k: int) (t: int) (n: int) =
    let random = Random()
    
    // Initialize random motifs
    let motifs = Array.init t (fun i ->
        let position = random.Next(0, dna.[i].Length - k + 1)
        dna.[i].Substring(position, k)
    )
    
    let bestMotifs = Array.copy motifs
    let mutable bestScore = calculateMotifScore motifs
    
    for _ in 0..(n - 1) do
        // Choose random sequence to remove
        let randomSequenceIndex = random.Next(0, t)
        
        // Build profile from remaining motifs
        let profile = Array.zeroCreate k
        for i in 0..(k - 1) do
            profile.[i] <- Map.empty
        
        for i in 0..(t - 1) do
            if i <> randomSequenceIndex then
                let motif = motifs.[i]
                for j in 0..(k - 1) do
                    let nucleotide = motif.[j]
                    let currentProfile = profile.[j]
                    let newProfile = 
                        if Map.containsKey nucleotide currentProfile then
                            currentProfile |> Map.add nucleotide (currentProfile.[nucleotide] + 1.0)
                        else
                            currentProfile |> Map.add nucleotide 1.0
                    profile.[j] <- newProfile
        
        // Normalize profile
        for i in 0..(k - 1) do
            let total = profile.[i].Values |> Seq.sum
            if total > 0.0 then
                profile.[i] <- profile.[i] |> Map.map (fun _ value -> value / total)
        
        // Generate new motif for the removed sequence
        let sequence = dna.[randomSequenceIndex]
        let newMotif = 
            let probabilities = Array.zeroCreate (sequence.Length - k + 1)
            for i in 0..(sequence.Length - k) do
                let kmer = sequence.Substring(i, k)
                let prob = 
                    kmer 
                    |> Seq.mapi (fun j nuc -> 
                        let profileForPosition = profile.[j]
                        if Map.containsKey nuc profileForPosition then
                            profileForPosition.[nuc]
                        else
                            0.0)
                    |> Seq.product
                probabilities.[i] <- prob
            
            // Sample based on probabilities
            let total = probabilities |> Array.sum
            if total > 0.0 then
                let rand = random.NextDouble() * total
                let mutable cumulative = 0.0
                let mutable newPosition = 0
                for i in 0..(sequence.Length - k) do
                    cumulative <- cumulative + probabilities.[i]
                    if cumulative >= rand then
                        newPosition <- i
                        break
                sequence.Substring(newPosition, k)
            else
                // If all probabilities are zero, choose randomly
                let position = random.Next(0, sequence.Length - k + 1)
                sequence.Substring(position, k)
        
        motifs.[randomSequenceIndex] <- newMotif
        
        // Update best motifs
        let currentScore = calculateMotifScore motifs
        if currentScore < bestScore then
            bestScore <- currentScore
            Array.blit motifs 0 bestMotifs 0 t
    
    bestMotifs

// Helper function to print motifs
let printMotifs (motifs: string[]) =
    String.concat " " motifs

// Example usage:
// let dna = [| "CGCCCCTCAGGGTCTACACCATGAGAGG"; 
//              "GGGCGAGGTATGTGTAAGTGCCAAGGTGCCAG"; 
//              "TAGTACCGAGACCGAAAGAAGTATACAGGCGG"; 
//              "TTTTTCTGTGAGGCTCAGGAGCCTCCCGACTT"; 
//              "GAGCGTTAGGCGCGACCTGGATTAGGAAGC" |]
// let result = gibbsSamplerClean dna 7 5 20
// printfn "%s" (printMotifs result)
```

## Key Components

1. **Initialization**: Randomly select k-mers from each DNA sequence
2. **Profile Construction**: Build a probability profile from the remaining motifs
3. **Sampling**: Choose new motif position based on profile probabilities
4. **Optimization**: Keep track of the best motifs found so far
5. **Iteration**: Repeat for the specified number of iterations

## Time Complexity
- O(n × t × k) per iteration
- Overall: O(n × t × k × iterations)

## Space Complexity
- O(t × k) for storing motifs and profile

This implementation follows the standard Gibbs sampling approach for motif finding, where we iteratively improve our motif set by sampling new positions based on a probabilistic profile.

