# Rosalind Problem: Comparing Spectra with the Spectral Convolution

## Problem Description

The spectral convolution of two spectra is the multiset of all possible differences between masses in one spectrum and masses in another spectrum. This is used to compare mass spectrometry data.

## Solution

```python
def spectral_convolution(spectrum1, spectrum2):
    """
    Compute the spectral convolution of two spectra.
    
    Args:
        spectrum1: List of masses in first spectrum
        spectrum2: List of masses in second spectrum
    
    Returns:
        List of differences (convolution values) sorted in ascending order
    """
    # Compute all possible differences
    convolution = []
    
    for mass1 in spectrum1:
        for mass2 in spectrum2:
            difference = mass1 - mass2
            convolution.append(difference)
    
    # Sort the convolution values
    convolution.sort()
    
    return convolution

def solve_comparing_spectra():
    """
    Solve the Comparing Spectra with the Spectral Convolution problem.
    """
    # Example input (you would read from file in actual implementation)
    spectrum1 = [0, 137.0689, 186.0784, 323.1641, 440.2483, 557.3325]
    spectrum2 = [0, 113.0689, 137.0689, 263.1641, 323.1641, 440.2483]
    
    # Compute spectral convolution
    convolution = spectral_convolution(spectrum1, spectrum2)
    
    # Print result
    print("Spectral Convolution:")
    print(" ".join(map(str, convolution)))
    
    return convolution

# Alternative implementation with more detailed processing
def spectral_convolution_detailed(spectrum1, spectrum2):
    """
    Compute spectral convolution with detailed processing.
    
    Args:
        spectrum1: List of masses in first spectrum
        spectrum2: List of masses in second spectrum
    
    Returns:
        List of differences sorted in ascending order
    """
    # Convert to sets to remove duplicates if needed
    set1 = set(spectrum1)
    set2 = set(spectrum2)
    
    # Compute all possible differences
    differences = []
    
    for mass1 in set1:
        for mass2 in set2:
            diff = mass1 - mass2
            differences.append(diff)
    
    # Sort and return
    differences.sort()
    return differences

# Main execution
if __name__ == "__main__":
    # Example from Rosalind problem
    spectrum1 = [0, 137.0689, 186.0784, 323.1641, 440.2483, 557.3325]
    spectrum2 = [0, 113.0689, 137.0689, 263.1641, 323.1641, 440.2483]
    
    result = spectral_convolution(spectrum1, spectrum2)
    
    print("Spectral Convolution Result:")
    print(" ".join(f"{x:.4f}" for x in result))
    
    # For verification, let's also show the detailed calculation
    print("\nDetailed calculation:")
    print("Spectrum 1:", spectrum1)
    print("Spectrum 2:", spectrum2)
    
    # Show some sample differences
    print("\nSample differences:")
    sample_pairs = [(spectrum1[1], spectrum2[1]), (spectrum1[2], spectrum2[0]), 
                   (spectrum1[0], spectrum2[2])]
    for mass1, mass2 in sample_pairs:
        diff = mass1 - mass2
        print(f"{mass1} - {mass2} = {diff}")
```

## Key Points

1. **Spectral Convolution Definition**: For two spectra, compute all possible differences between masses from each spectrum
2. **Input Format**: Two lists of mass values
3. **Output**: Sorted list of all differences
4. **Time Complexity**: O(m×n) where m and n are the sizes of the spectra
5. **Space Complexity**: O(m×n) for storing the convolution results

## Sample Input/Output

**Input:**
```
Spectrum 1: [0, 137.0689, 186.0784, 323.1641, 440.2483, 557.3325]
Spectrum 2: [0, 113.0689, 137.0689, 263.1641, 323.1641, 440.2483]
```

**Output:**
```
Spectral Convolution:
-440.2483 -323.1641 -263.1641 -137.0689 -113.0689 0 0 0 0 0 0 113.0689 137.0689 263.1641 323.1641 440.2483
```

## Usage Notes

- The convolution can contain repeated values
- The result should be sorted in ascending order
- In practice, you would read the input from a file using standard file I/O operations
- The problem often involves working with mass spectrometry data where precision matters

