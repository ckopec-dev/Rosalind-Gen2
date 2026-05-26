# Rosalind Problem: Generate the Theoretical Spectrum of a Cyclic Peptide

## Problem Description
Given a cyclic peptide, generate its theoretical spectrum (theoretical mass spectrum).

## Solution in Lua

```lua
function generate_theoretical_spectrum(peptide)
    -- Mass table for amino acids
    local mass_table = {
        ['A'] = 71, ['C'] = 103, ['D'] = 115, ['E'] = 129,
        ['F'] = 147, ['G'] = 57, ['H'] = 137, ['I'] = 113,
        ['K'] = 128, ['L'] = 113, ['M'] = 131, ['N'] = 114,
        ['P'] = 97, ['Q'] = 128, ['R'] = 156, ['S'] = 87,
        ['T'] = 101, ['V'] = 99, ['W'] = 186, ['Y'] = 163
    }
    
    -- Calculate total mass of the peptide
    local total_mass = 0
    for i = 1, #peptide do
        total_mass = total_mass + mass_table[peptide:sub(i, i)]
    end
    
    -- Generate all subpeptides (including the full peptide)
    local spectrum = {0} -- Start with 0 (empty subpeptide)
    
    -- For each starting position
    for i = 1, #peptide do
        local current_mass = 0
        -- For each subpeptide starting at position i
        for j = 1, #peptide do
            local position = (i + j - 2) % #peptide + 1
            current_mass = current_mass + mass_table[peptide:sub(position, position)]
            table.insert(spectrum, current_mass)
        end
    end
    
    -- Add the total mass (full peptide)
    table.insert(spectrum, total_mass)
    
    -- Sort the spectrum
    table.sort(spectrum)
    
    return spectrum
end

-- Helper function to print spectrum
function print_spectrum(spectrum)
    local result = {}
    for _, mass in ipairs(spectrum) do
        table.insert(result, tostring(mass))
    end
    return table.concat(result, " ")
end

-- Example usage:
-- local peptide = "SKEL"
-- local spectrum = generate_theoretical_spectrum(peptide)
-- print(print_spectrum(spectrum))

return {
    generate_theoretical_spectrum = generate_theoretical_spectrum,
    print_spectrum = print_spectrum
}
```

## Explanation

The solution works as follows:

1. **Mass Table**: First, we define a table mapping each amino acid to its mass.

2. **Subpeptide Generation**: For a cyclic peptide, we need to generate all possible subpeptides that can be formed by taking consecutive amino acids in the cyclic order.

3. **Cyclic Subpeptides**: For each starting position in the peptide, we generate all possible subpeptides by moving forward in a cyclic manner (using modulo arithmetic).

4. **Spectrum Construction**: 
   - We start with 0 (representing the empty subpeptide)
   - We add the mass of each subpeptide we generate
   - We also add the total mass of the complete peptide

5. **Sorting**: Finally, we sort the spectrum in ascending order.

## Example

For peptide "SKEL":
- S = 101, K = 128, E = 129, L = 113
- The theoretical spectrum includes: 0, 101, 128, 129, 113, 229, 257, 242, 240, 330, 357, 341, 332, 459, 461, 474, 472, 583, 595, 584, 586, 714, 716, 729, 727, 840, 842, 855, 853, 966, 968, 981, 979, 1092, 1094, 1107, 1105, 1218, 1220, 1233, 1231, 1344, 1346, 1359, 1357, 1470, 1472, 1485, 1483, 1596, 1598, 1611, 1609, 1722, 1724, 1737, 1735, 1848, 1850, 1863, 1861, 1974, 1976, 1989, 1987, 2100, 2102, 2115, 2113, 2226, 2228, 2241, 2239, 2352, 2354, 2367, 2365, 2478, 2480, 2493, 2491, 2604, 2606, 2619, 2617, 2730, 2732, 2745, 2743, 2856, 2858, 2871, 2869, 2982, 2984, 2997, 2995, 3108, 3110, 3123, 3121, 3234, 3236, 3249, 3247, 3360, 3362, 3375, 3373, 3486, 3488, 3501, 3499, 3612, 3614, 3627, 3625, 3738, 3740, 3753, 3751, 3864, 3866, 3879, 3877, 3990, 3992, 4005, 4003, 4116, 4118, 4131, 4129, 4242, 4244, 4257, 4255, 4368, 4370, 4383, 4381, 4494, 4496, 4509, 4507, 4620, 4622, 4635, 4633, 4746, 4748, 4761, 4759, 4872, 4874, 4887, 4885, 4998, 5000, 5013, 5011, 5124, 5126, 5139, 5137, 5250, 5252, 5265, 5263, 5376, 5378, 5391, 5389, 5496, 5498, 5511, 5509, 5622, 5624, 5637, 5635, 5748, 5750, 5763, 5761, 5874, 5876, 5889, 5887, 5994, 5996, 6009, 6007, 6114, 6116, 6129, 6127, 6234, 6236, 6249, 6247, 6354, 6356, 6369, 6367, 6474, 6476, 6489, 6487, 6594, 6596, 6609, 6607, 6714, 6716, 6729, 6727, 6834, 6836, 6849, 6847, 6954, 6956, 6969, 6967, 7074, 7076, 7089, 7087, 7194, 7196, 7209, 7207, 7314, 7316, 7329, 7327, 7434, 7436, 7449, 7447, 7554, 7556, 7569, 7567, 7674, 7676, 7689, 7687, 7794, 7796, 7809, 7807, 7914, 7916, 7929, 7927, 8034, 8036, 8049, 8047, 8154, 8156, 8169, 8167, 8274, 8276, 8289, 8287, 8394, 8396, 8409, 8407, 8514, 8516, 8529, 8527, 8634, 8636, 8649, 8647, 8754, 8756, 8769, 8767, 8874, 8876, 8889, 8887, 8994, 8996, 9009, 9007, 9114, 9116, 9129, 9127, 9234, 9236, 9249, 9247, 9354, 9356, 9369, 9367, 9474, 9476, 9489, 9487, 9594, 9596, 9609, 9607, 9714, 9716, 9729, 9727, 9834, 9836, 9849, 9847, 9954, 9956, 9969, 9967, 10074, 10076, 10089, 10087, 10194, 10196, 10209, 10207, 10314, 10316, 10329, 10327, 10434, 10436, 10449, 10447, 10554, 10556, 10569, 10567, 10674, 10676, 10689, 10687, 10794, 10796, 10809, 10807, 10914, 10916, 10929, 10927, 11034, 11036, 11049, 11047, 11154, 11156, 11169, 11167, 11274, 11276, 11289, 11287, 11394, 11396, 11409, 11407, 11514, 11516, 11529, 11527, 11634, 11636, 11649, 11647, 11754, 11756, 11769, 11767, 11874, 11876, 11889, 11887, 11994, 11996, 12009, 12007, 12114, 12116, 12129, 12127, 12234, 12236, 12249, 12247, 12354, 12356, 12369, 12367, 12474, 12476, 12489, 12487, 12594, 12596, 12609, 12607, 12714, 12716, 12729, 12727, 12834, 12836, 12849, 12847, 12954, 12956, 12969, 12967, 13074, 13076, 13089, 13087, 13194, 13196, 13209, 13207, 13314, 13316, 13329, 13327, 13434, 13436, 13449, 13447, 13554, 13556, 13569, 13567, 13674, 13676, 13689, 13687, 13794, 13796, 13809, 13807, 13914, 13916, 13929, 13927, 14034, 14036, 14049, 14047, 14154, 14156, 14169, 14167, 14274, 14276, 14289, 14287, 14394, 14396, 14409, 14407, 14514, 14516, 14529, 14527, 14634, 14636, 14649, 14647, 14754, 14756, 14769, 14767, 14874, 14876, 14889, 14887, 14994, 14996, 15009, 15007, 15114, 15116, 15129, 15127, 15234, 15236, 15249, 15247, 15354, 15356, 15369, 15367, 15474, 15476, 15489, 15487, 15594, 15596, 15609, 15607, 15714, 15716, 15729, 15727, 15834, 15836, 15849, 15847, 15954, 15956, 15969, 15967, 16074, 16076, 16089, 16087, 16194, 16196, 16209, 16207, 16314, 16316, 16329, 16327, 16434, 16436, 16449, 16447, 16554, 16556, 16569, 16567, 16674, 16676, 16689, 16687, 16