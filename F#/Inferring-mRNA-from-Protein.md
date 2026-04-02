# Rosalind Problem: Inferring_mRNA_from_Protein (F# Solution)

## Problem Understanding

The task is to calculate how many different mRNA strings could encode a given protein sequence, considering that:
- Each amino acid can be encoded by multiple codons (triplets of nucleotides)
- The stop codon (UAA, UAG, UGA) has a special count of 3
- The result should be taken modulo 1,000,000

## Solution Approach

1. Create a mapping from amino acids to their codon counts
2. For each amino acid in the protein sequence, multiply the result by the number of possible codons
3. Handle the stop codon separately (3 possibilities)
4. Apply modulo arithmetic to prevent overflow

## F# Implementation

```fsharp
open System

let inferringMRNAFromProtein (protein: string) : int =
    // Codon count mapping for each amino acid
    let codonCounts = Map [
        ('A', 4)  // Alanine
        ('C', 2)  // Cysteine
        ('D', 2)  // Aspartic acid
        ('E', 2)  // Glutamic acid
        ('F', 2)  // Phenylalanine
        ('G', 4)  // Glycine
        ('H', 2)  // Histidine
        ('I', 3)  // Isoleucine
        ('K', 2)  // Lysine
        ('L', 6)  // Leucine
        ('M', 1)  // Methionine
        ('N', 2)  // Asparagine
        ('P', 4)  // Proline
        ('Q', 2)  // Glutamine
        ('R', 6)  // Arginine
        ('S', 6)  // Serine
        ('T', 4)  // Threonine
        ('V', 4)  // Valine
        ('W', 1)  // Tryptophan
        ('Y', 2)  // Tyrosine
    ]
    
    // Stop codon has 3 possibilities
    let stopCodons = 3
    
    // Initialize result
    let mutable result = 1L
    
    // Process each amino acid in the protein
    for aminoAcid in protein do
        match codonCounts.TryFind(aminoAcid) with
        | Some count -> 
            result <- (result * int64 count) % 1000000L
        | None -> 
            // Handle unknown amino acids if any
            failwith "Unknown amino acid"
    
    // Multiply by stop codon possibilities
    result <- (result * int64 stopCodons) % 1000000L
    
    int result

// Alternative implementation using List.fold
let inferringMRNAFromProteinFold (protein: string) : int =
    let codonCounts = Map [
        ('A', 4)  ('C', 2)  ('D', 2)  ('E', 2)  ('F', 2)
        ('G', 4)  ('H', 2)  ('I', 3)  ('K', 2)  ('L', 6)
        ('M', 1)  ('N', 2)  ('P', 4)  ('Q', 2)  ('R', 6)
        ('S', 6)  ('T', 4)  ('V', 4)  ('W', 1)  ('Y', 2)
    ]
    
    let stopCodons = 3L
    
    let result = 
        protein
        |> Seq.fold (fun acc aminoAcid ->
            let count = codonCounts.[aminoAcid]
            (acc * int64 count) % 1000000L
        ) 1L
    
    int ((result * stopCodons) % 1000000L)

// Example usage
let exampleProtein = "MA"
let result = inferringMRNAFromProtein exampleProtein
printfn "Number of possible mRNA strings: %d" result
```

## Explanation

The solution works as follows:

1. **Codon Count Mapping**: We create a mapping from each amino acid to the number of possible codons that can encode it.

2. **Process Each Amino Acid**: For each amino acid in the protein sequence, we multiply the current result by the number of possible codons for that amino acid.

3. **Handle Stop Codon**: Since we need to end the protein sequence, we multiply by 3 (representing the three possible stop codons: UAA, UAG, UGA).

4. **Modular Arithmetic**: We apply modulo 1,000,000 at each step to prevent integer overflow and match the expected output format.

5. **Return Result**: Convert the final result back to int and return it.

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the length of the protein sequence
- **Space Complexity**: O(1) as we use a fixed-size map and only store a few variables

## Test Case

For the example protein "MA":
- M (Methionine) → 1 codon
- A (Alanine) → 4 codons  
- Stop codon → 3 possibilities
- Total = 1 × 4 × 3 = 12

The function returns 12 as the number of possible mRNA strings that could encode the protein "MA".

