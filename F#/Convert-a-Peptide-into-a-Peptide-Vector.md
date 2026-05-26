# Rosalind Problem: Convert a Peptide into a Peptide Vector (F# Solution)

## Problem Description
Given a peptide, we need to convert it into a peptide vector. The peptide vector is a binary vector of length 3n where n is the length of the peptide, such that:
- Each amino acid is represented by a 3-bit binary vector
- The vector has 1s at positions corresponding to the amino acid's mass
- The mass of each amino acid is given in a predefined mapping

## Solution

```fsharp
open System

// Amino acid mass to index mapping
let aminoAcidMasses = 
    Map [
        'A', 71  // Alanine
        'C', 103 // Cysteine
        'D', 115 // Aspartic acid
        'E', 129 // Glutamic acid
        'F', 147 // Phenylalanine
        'G', 57  // Glycine
        'H', 137 // Histidine
        'I', 113 // Isoleucine
        'K', 128 // Lysine
        'L', 113 // Leucine
        'M', 131 // Methionine
        'N', 114 // Asparagine
        'P', 97  // Proline
        'Q', 128 // Glutamine
        'R', 156 // Arginine
        'S', 87  // Serine
        'T', 101 // Threonine
        'V', 99  // Valine
        'W', 186 // Tryptophan
        'Y', 163 // Tyrosine
    ]

// Convert peptide to peptide vector
let peptideToVector (peptide: string) : int list =
    let n = peptide.Length
    let vectorLength = 3 * n
    let vector = Array.create vectorLength 0
    
    // Process each amino acid in the peptide
    for i = 0 to n - 1 do
        let aminoAcid = peptide.[i]
        let mass = aminoAcidMasses.[aminoAcid]
        
        // Convert mass to index in vector (0-based)
        // Mass values are 1-indexed, so we subtract 1
        let index = mass - 1
        
        // Set the corresponding bit to 1
        if index < vectorLength then
            vector.[index] <- 1
    
    // Convert array to list
    vector |> Array.toList

// Alternative implementation using List.map and List.fold
let peptideToVectorAlt (peptide: string) : int list =
    let n = peptide.Length
    let vectorLength = 3 * n
    let vector = Array.create vectorLength 0
    
    peptide
    |> Seq.iteri (fun i aminoAcid ->
        let mass = aminoAcidMasses.[aminoAcid]
        let index = mass - 1
        if index < vectorLength then
            vector.[index] <- 1)
    
    vector |> Array.toList

// Example usage and test
let testPeptide = "LEQN"
let result = peptideToVector testPeptide

printfn "Peptide: %s" testPeptide
printfn "Peptide vector: %A" result

// Test with another example
let testPeptide2 = "G"
let result2 = peptideToVector testPeptide2
printfn "Peptide: %s" testPeptide2
printfn "Peptide vector: %A" result2
```

## Explanation

The solution works as follows:

1. **Mass Mapping**: We create a mapping from amino acid characters to their respective masses.

2. **Vector Construction**: For a peptide of length n, we create a vector of length 3n (since each amino acid contributes 3 bits).

3. **Bit Setting**: For each amino acid in the peptide:
   - Get its mass from the mapping
   - Convert the mass to an index in the vector (subtract 1 since indexing starts at 0)
   - Set the corresponding position in the vector to 1

4. **Return Result**: Convert the array to a list for output.

## Key Points

- The vector length is 3 × peptide length
- Each amino acid contributes to a specific bit position based on its mass
- The mass values are used directly as indices (adjusted for 0-based indexing)
- The solution handles the conversion from amino acid sequence to binary vector representation

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the peptide
- **Space Complexity**: O(3n) for the output vector

This approach efficiently converts any peptide string into its corresponding peptide vector representation as required by the Rosalind problem.

