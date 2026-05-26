# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description
The spectral convolution of two spectra is the multiset of differences between their masses. Given two spectra, we need to compute their spectral convolution.

## Solution

```kotlin
import java.io.File
import kotlin.math.abs

fun main() {
    // Read input from file
    val input = File("input.txt").readLines()
    
    // Parse the spectra
    val spectrum1 = input[0].split(" ").map { it.toDouble() }
    val spectrum2 = input[1].split(" ").map { it.toDouble() }
    
    // Compute spectral convolution
    val convolution = computeSpectralConvolution(spectrum1, spectrum2)
    
    // Output results
    println(convolution.joinToString(" "))
}

fun computeSpectralConvolution(spectrum1: List<Double>, spectrum2: List<Double>): List<Double> {
    val convolution = mutableListOf<Double>()
    
    // For each mass in spectrum1, subtract each mass in spectrum2
    for (mass1 in spectrum1) {
        for (mass2 in spectrum2) {
            val difference = abs(mass1 - mass2)
            convolution.add(difference)
        }
    }
    
    // Sort the convolution results
    return convolution.sorted()
}
```

## Explanation

1. **Input Parsing**: The program reads two spectra from input, each represented as a list of mass values.

2. **Spectral Convolution Calculation**: 
   - For each mass in the first spectrum, we subtract every mass in the second spectrum
   - We take the absolute value of the difference to ensure non-negative results
   - All differences are collected in a list

3. **Sorting**: The convolution results are sorted in ascending order as required by the problem.

## Example

If the input is:
```
0 2 4 6
1 3 5 7
```

The convolution would be:
- From 0: |0-1|=1, |0-3|=3, |0-5|=5, |0-7|=7
- From 2: |2-1|=1, |2-3|=1, |2-5|=3, |2-7|=5
- From 4: |4-1|=3, |4-3|=1, |4-5|=1, |4-7|=3
- From 6: |6-1|=5, |6-3|=3, |6-5|=1, |6-7|=1

Sorted results: `1 1 1 1 3 3 3 3 5 5 5 7`

## Time Complexity
- **Time**: O(m × n) where m and n are the sizes of the two spectra
- **Space**: O(m × n) for storing the convolution results

The solution handles the spectral convolution computation efficiently by directly calculating all pairwise differences between masses in the two spectra.

