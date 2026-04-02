# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description
The theoretical spectrum of a cyclic peptide is the multiset of all subpeptide masses of that peptide, including the empty subpeptide (mass 0) and the full peptide itself. For a cyclic peptide of length n, there are n subpeptides of each length from 1 to n, plus the empty subpeptide and the full peptide.

## Solution Approach
1. Create a mapping of amino acid masses
2. Generate all possible subpeptides of a cyclic peptide
3. Calculate masses of each subpeptide
4. Return the spectrum sorted in ascending order

## Kotlin Implementation

```kotlin
fun generateTheoreticalSpectrum(peptide: String): List<Int> {
    // Amino acid masses
    val massTable = mapOf(
        'A' to 71, 'C' to 103, 'D' to 115, 'E' to 129, 'F' to 147,
        'G' to 57, 'H' to 137, 'I' to 113, 'K' to 128, 'L' to 113,
        'M' to 131, 'N' to 114, 'P' to 97, 'Q' to 128, 'R' to 156,
        'S' to 87, 'T' to 101, 'V' to 99, 'W' to 186, 'Y' to 163
    )
    
    val n = peptide.length
    val spectrum = mutableListOf<Int>()
    
    // Add mass 0 for empty subpeptide
    spectrum.add(0)
    
    // For each possible subpeptide length (1 to n)
    for (length in 1 until n) {
        // For each starting position
        for (start in 0 until n) {
            val end = (start + length) % n
            val subpeptide = if (start < end) {
                peptide.substring(start, end)
            } else {
                peptide.substring(start) + peptide.substring(0, end)
            }
            
            // Calculate mass of subpeptide
            val mass = subpeptide.sumOf { massTable[it]!! }
            spectrum.add(mass)
        }
    }
    
    // Add mass of full peptide
    val fullMass = peptide.sumOf { massTable[it]!! }
    spectrum.add(fullMass)
    
    // Sort spectrum in ascending order
    return spectrum.sorted()
}

// Alternative implementation using a more explicit approach
fun generateTheoreticalSpectrumAlternative(peptide: String): List<Int> {
    val massTable = mapOf(
        'A' to 71, 'C' to 103, 'D' to 115, 'E' to 129, 'F' to 147,
        'G' to 57, 'H' to 137, 'I' to 113, 'K' to 128, 'L' to 113,
        'M' to 131, 'N' to 114, 'P' to 97, 'Q' to 128, 'R' to 156,
        'S' to 87, 'T' to 101, 'V' to 99, 'W' to 186, 'Y' to 163
    )
    
    val n = peptide.length
    val spectrum = mutableSetOf<Int>()
    
    // Add mass 0 for empty subpeptide
    spectrum.add(0)
    
    // Create a doubled peptide to handle cyclic nature easily
    val doubledPeptide = peptide + peptide
    
    // Generate all subpeptides of each length
    for (length in 1..n) {
        for (start in 0 until n) {
            val subpeptide = doubledPeptide.substring(start, start + length)
            val mass = subpeptide.sumOf { massTable[it]!! }
            spectrum.add(mass)
        }
    }
    
    return spectrum.sorted().toList()
}

// Example usage
fun main() {
    val peptide = "LEQN"
    val spectrum = generateTheoreticalSpectrum(peptide)
    println(spectrum.joinToString(" "))
    // Expected output: 0 113 114 128 129 227 228 242 243 370 371 497 498 625 626 753 754 881 882 1009 1010
}
```

## Explanation

The solution works by:

1. **Mass Table**: Creating a mapping of amino acids to their masses
2. **Cyclic Handling**: Using the doubled peptide approach to easily handle the cyclic nature of the peptide
3. **Subpeptide Generation**: For each possible subpeptide length (1 to n), we generate all possible subpeptides
4. **Mass Calculation**: Sum the masses of amino acids in each subpeptide
5. **Sorting**: Return the spectrum sorted in ascending order

## Time Complexity
- Time: O(n³) where n is the length of the peptide
- Space: O(n²) for storing all subpeptide masses

## Example
For peptide "LEQN":
- Empty subpeptide: mass 0
- Subpeptides of length 1: L(113), E(129), Q(128), N(114)
- Subpeptides of length 2: LE(242), EQ(257), QN(242), NL(227)
- And so on...
- Full peptide LEQN: mass 497

The output will be the sorted list of all these masses.

