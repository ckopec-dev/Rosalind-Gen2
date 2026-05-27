# Rosalind Problem: Inferring mRNA from Protein

## Problem Description
Given an amino acid string, determine the number of possible mRNA strings from which the protein could have been translated, modulo 1000000.

## Solution Approach
1. Each amino acid can be translated from multiple codons (3-letter RNA sequences)
2. For each amino acid, count how many codons can translate to it
3. Multiply the possibilities for each amino acid
4. Apply modulo 1000000 to prevent overflow

## Scala Implementation

```scala
object InferringMRNAFromProtein {
  
  def main(args: Array[String]): Unit = {
    // Codon table mapping amino acids to their possible codons
    val codonTable = Map(
      'A' -> 4, 'C' -> 2, 'D' -> 2, 'E' -> 2, 'F' -> 2,
      'G' -> 4, 'H' -> 2, 'I' -> 3, 'K' -> 2, 'L' -> 6,
      'M' -> 1, 'N' -> 2, 'P' -> 4, 'Q' -> 2, 'R' -> 6,
      'S' -> 6, 'T' -> 4, 'V' -> 4, 'W' -> 1, 'Y' -> 2,
      '*' -> 3  // Stop codons
    )
    
    // Read input protein string
    val protein = scala.io.StdIn.readLine()
    
    // Calculate total possibilities
    val result = protein.foldLeft(1L) { (acc, aminoAcid) =>
      val possibilities = codonTable(aminoAcid)
      (acc * possibilities) % 1000000
    }
    
    println(result)
  }
}
```

## Explanation

The solution works as follows:

1. **Codon Table**: We create a map where each amino acid maps to the number of possible codons that can translate to it
2. **Input Processing**: Read the protein string from standard input
3. **Calculation**: For each amino acid in the protein string:
   - Look up the number of possible codons for that amino acid
   - Multiply this by the running total
   - Apply modulo 1000000 at each step to prevent integer overflow
4. **Output**: Print the final result

## Key Points

- The stop codon `*` has 3 possible codons (UAA, UAG, UGA)
- We use `Long` type to handle potential overflow during multiplication
- Modular arithmetic is applied at each step to keep numbers manageable
- The order of amino acids in the protein string determines the sequence of multiplications

## Example
For protein string "MA":
- M can be encoded by 1 codon → 1 possibility
- A can be encoded by 4 codons → 4 possibilities
- Total = 1 × 4 = 4

The solution efficiently handles the combinatorial nature of the problem while maintaining numerical stability through modular arithmetic.

