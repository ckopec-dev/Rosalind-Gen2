# Rosalind Problem: Matching a Spectrum to a Protein

## Problem Description
Given a cyclic spectrum and a protein string, determine if the protein's theoretical spectrum matches the given spectrum.

## Solution Approach
1. Generate the theoretical spectrum of the given protein
2. Compare it with the given spectrum
3. Return whether they match

## Implementation

```javascript
// Function to calculate the theoretical spectrum of a protein
function calculateTheoreticalSpectrum(protein) {
    // Amino acid masses
    const masses = {
        'A': 71.03711,
        'C': 103.00919,
        'D': 115.02694,
        'E': 129.04259,
        'F': 147.06841,
        'G': 57.02130,
        'H': 137.05891,
        'I': 113.08406,
        'K': 128.09496,
        'L': 113.08406,
        'M': 131.04049,
        'N': 114.04293,
        'P': 97.05276,
        'Q': 128.05858,
        'R': 156.10111,
        'S': 87.03203,
        'T': 101.04768,
        'V': 99.06841,
        'W': 186.07931,
        'Y': 163.06333
    };
    
    // Calculate prefix masses
    let prefixMasses = [0];
    for (let i = 0; i < protein.length; i++) {
        prefixMasses.push(prefixMasses[i] + masses[protein[i]]);
    }
    
    // Generate all subpeptides
    let spectrum = [0];
    
    for (let i = 0; i < protein.length; i++) {
        for (let j = i + 1; j <= protein.length; j++) {
            // Linear subpeptide
            let mass = prefixMasses[j] - prefixMasses[i];
            spectrum.push(mass);
            
            // If it's a cyclic peptide, also consider the wrap-around
            if (j - i < protein.length) {
                let mass2 = prefixMasses[protein.length] - prefixMasses[j] + prefixMasses[i];
                spectrum.push(mass2);
            }
        }
    }
    
    // Sort the spectrum
    spectrum.sort((a, b) => a - b);
    return spectrum;
}

// Function to check if two spectra match
function spectraMatch(spectrum1, spectrum2) {
    // Sort both spectra
    spectrum1.sort((a, b) => a - b);
    spectrum2.sort((a, b) => a - b);
    
    // Check if they have the same length
    if (spectrum1.length !== spectrum2.length) {
        return false;
    }
    
    // Check if all elements match
    for (let i = 0; i < spectrum1.length; i++) {
        if (Math.abs(spectrum1[i] - spectrum2[i]) > 0.001) { // Using small epsilon for floating point comparison
            return false;
        }
    }
    
    return true;
}

// Main function to solve the problem
function matchingASpectrumToAProtein(spectrum, protein) {
    // Calculate theoretical spectrum of the protein
    const theoreticalSpectrum = calculateTheoreticalSpectrum(protein);
    
    // Check if they match
    return spectraMatch(spectrum, theoreticalSpectrum);
}

// Example usage:
// const spectrum = [0, 113, 128, 186, 244, 299, 314, 372, 427, 442, 500, 515];
// const protein = "SKADYEK";
// console.log(matchingASpectrumToAProtein(spectrum, protein));
```

## Explanation

1. **Amino Acid Masses**: The solution uses a lookup table containing the masses of all 20 amino acids.

2. **Prefix Masses**: We calculate prefix masses to efficiently compute subpeptide masses.

3. **Spectrum Generation**: For a cyclic peptide, we generate all possible subpeptides and their masses:
   - Linear subpeptides (from i to j)
   - Cyclic subpeptides (wrapping around the peptide)

4. **Spectrum Comparison**: We sort both spectra and compare element by element with a small epsilon for floating-point precision.

5. **Time Complexity**: O(n²) where n is the length of the protein string.

This solution handles the cyclic nature of protein spectra and properly compares the theoretical spectrum with the given spectrum to determine if they match.

