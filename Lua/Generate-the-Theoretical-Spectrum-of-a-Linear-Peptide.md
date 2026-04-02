# Rosalind Problem: Generate the Theoretical Spectrum of a Linear Peptide

## Problem Description
The theoretical spectrum of a linear peptide is the multiset of all subpeptide masses of the peptide, including the empty subpeptide (mass 0) and the full peptide itself.

## Solution in Lua

```lua
function get_amino_acid_masses()
    -- Return a table mapping amino acid letters to their masses
    return {
        A = 71, B = 132, C = 103, D = 115, E = 129,
        F = 147, G = 57,  H = 137, I = 113, K = 128,
        L = 113, M = 131, N = 114, P = 97,  Q = 128,
        R = 156, S = 87,  T = 101, V = 99,  W = 186,
        Y = 163, Z = 132
    }
end

function generate_theoretical_spectrum(peptide)
    local amino_acid_masses = get_amino_acid_masses()
    local spectrum = {0} -- Start with mass 0 (empty subpeptide)
    
    -- Generate all possible subpeptides
    for i = 1, #peptide do
        local mass = 0
        for j = i, #peptide do
            mass = mass + amino_acid_masses[peptide:sub(j, j)]
            table.insert(spectrum, mass)
        end
    end
    
    -- Sort the spectrum
    table.sort(spectrum)
    
    return spectrum
end

-- Example usage
local peptide = "NQEL"
local spectrum = generate_theoretical_spectrum(peptide)

-- Print the result
local result = {}
for i, mass in ipairs(spectrum) do
    table.insert(result, tostring(mass))
end
print(table.concat(result, " "))

-- Alternative: if reading from input file
function solve_from_input(input_file)
    local file = io.open(input_file, "r")
    if not file then
        error("Could not open file")
    end
    
    local peptide = file:read("*line")
    file:close()
    
    local spectrum = generate_theoretical_spectrum(peptide)
    
    local result = {}
    for i, mass in ipairs(spectrum) do
        table.insert(result, tostring(mass))
    end
    
    return table.concat(result, " ")
end

-- Test with the example
print("Example: NQEL")
print(generate_theoretical_spectrum("NQEL"))
```

## Explanation

1. **Amino Acid Masses**: The function `get_amino_acid_masses()` creates a lookup table mapping each amino acid letter to its mass.

2. **Spectrum Generation**: 
   - Start with mass 0 (empty subpeptide)
   - For each starting position in the peptide, generate all subpeptides starting from that position
   - Calculate the mass of each subpeptide by summing the masses of its amino acids
   - Add each subpeptide mass to the spectrum

3. **Sorting**: The final spectrum is sorted in ascending order as required.

## Example
For peptide "NQEL":
- Empty subpeptide: 0
- N: 114
- NQ: 114 + 128 = 242
- NQE: 114 + 128 + 129 = 371
- NQEL: 114 + 128 + 129 + 101 = 472
- Q: 128
- QE: 128 + 129 = 257
- QEL: 128 + 129 + 101 = 358
- E: 129
- EL: 129 + 101 = 230
- L: 101

Sorted spectrum: [0, 101, 114, 128, 129, 230, 242, 257, 358, 371, 472]

## Time Complexity
- O(n²) where n is the length of the peptide
- We generate n(n+1)/2 subpeptides, each taking O(1) time to compute

## Space Complexity
- O(n²) for storing all subpeptide masses

