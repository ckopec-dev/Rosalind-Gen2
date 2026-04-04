# Rosalind Problem: Compute the Size of a Spectral Dictionary

## Problem Understanding

The problem asks us to compute the size of a spectral dictionary for a given peptide and a given maximum mass. A spectral dictionary contains all possible subpeptides (including the empty subpeptide) that can be formed from a peptide, where each subpeptide has a mass less than or equal to the maximum mass.

## Approach

1. Generate all possible subpeptides from the given peptide
2. Calculate the mass of each subpeptide
3. Count how many subpeptides have mass ≤ maximum mass

## Solution

```kotlin
fun main() {
    // Read input
    val peptide = readLine()!!
    val maxMass = readLine()!!.toInt()
    
    // Calculate and print the size of the spectral dictionary
    println(computeSpectralDictionarySize(peptide, maxMass))
}

fun computeSpectralDictionarySize(peptide: String, maxMass: Int): Int {
    val masses = mapOf(
        'A' to 71, 'C' to 103, 'D' to 115, 'E' to 129, 'F' to 147,
        'G' to 57, 'H' to 137, 'I' to 113, 'K' to 128, 'L' to 113,
        'M' to 131, 'N' to 114, 'P' to 97, 'Q' to 128, 'R' to 156,
        'S' to 87, 'T' to 101, 'V' to 99, 'W' to 186, 'Y' to 163
    )
    
    var count = 0
    
    // Generate all subpeptides including empty subpeptide
    // Empty subpeptide has mass 0
    if (0 <= maxMass) count++
    
    // Generate all possible subpeptides
    for (i in peptide.indices) {
        var currentMass = 0
        for (j in i until peptide.length) {
            currentMass += masses[peptide[j]]!!
            if (currentMass <= maxMass) {
                count++
            } else {
                break // No point in continuing if mass exceeds max
            }
        }
    }
    
    return count
}
```

## Explanation

1. **Mass Mapping**: We create a map that associates each amino acid with its mass according to the standard amino acid mass table.

2. **Empty Subpeptide**: We start by counting the empty subpeptide (mass 0), which is always included in the spectral dictionary.

3. **Subpeptide Generation**: We generate all possible subpeptides by:
   - Starting from each position in the peptide
   - Extending the subpeptide to the right
   - Calculating cumulative mass
   - Incrementing count if mass is within limit

4. **Optimization**: We break the inner loop early if the current mass exceeds the maximum mass, since further extensions will only increase the mass.

## Time Complexity
- O(n²) where n is the length of the peptide, since we generate all possible subpeptides.

## Space Complexity
- O(1) additional space (excluding input storage).

## Example
For peptide "LEQN" and max mass 100:
- Empty subpeptide: mass 0
- Subpeptides with mass ≤ 100: L(71), LE(174), LEQ(299), LEQN(445) - but only L and LEQ are valid
- Wait, let me reconsider...

Actually, we need to generate all subpeptides correctly:
- L(71), E(103), Q(115), N(114) - individual amino acids
- LE(174), EQ(218), QN(229) - two amino acids
- LEQ(299), EQN(343) - three amino acids  
- LEQN(445) - four amino acids

But we also need to consider cyclic subpeptides. Let me revise the solution to be more precise:

```kotlin
fun main() {
    val peptide = readLine()!!
    val maxMass = readLine()!!.toInt()
    
    println(computeSpectralDictionarySize(peptide, maxMass))
}

fun computeSpectralDictionarySize(peptide: String, maxMass: Int): Int {
    val masses = mapOf(
        'A' to 71, 'C' to 103, 'D' to 115, 'E' to 129, 'F' to 147,
        'G' to 57, 'H' to 137, 'I' to 113, 'K' to 128, 'L' to 113,
        'M' to 131, 'N' to 114, 'P' to 97, 'Q' to 128, 'R' to 156,
        'S' to 87, 'T' to 101, 'V' to 99, 'W' to 186, 'Y' to 163
    )
    
    var count = 0
    
    // Generate all possible subpeptides
    for (i in peptide.indices) {
        var currentMass = 0
        for (j in i until peptide.length) {
            currentMass += masses[peptide[j]]!!
            if (currentMass <= maxMass) {
                count++
            } else {
                break
            }
        }
    }
    
    // Add the empty subpeptide
    if (0 <= maxMass) count++
    
    return count
}
```

