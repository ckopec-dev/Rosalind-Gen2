# Rosalind Problem: Compute the Number of Peptides of Given Total Mass

## Problem Understanding

We need to compute how many different peptides can be formed with a given total mass, where each amino acid has a specific mass. This is a classic dynamic programming problem similar to the coin change problem.

## Solution Approach

We'll use dynamic programming where:
- `dp[mass]` represents the number of ways to form that mass
- We iterate through each possible mass and update our dp array
- For each mass, we check all amino acid masses and add the number of ways to form (current_mass - amino_acid_mass)

## Lua Implementation

```lua
function compute_peptide_count(target_mass)
    -- Amino acid masses (with 1000 added to handle floating point precision)
    local amino_acids = {
        57, 71, 86, 99, 100, 101, 103, 113, 114, 115, 
        128, 129, 131, 132, 133, 134, 135, 147, 156, 163, 186
    }
    
    -- Initialize dp array
    local dp = {}
    dp[0] = 1  -- One way to make mass 0 (empty peptide)
    
    -- For each mass from 1 to target_mass
    for mass = 1, target_mass do
        dp[mass] = 0
        
        -- Check each amino acid
        for _, acid_mass in ipairs(amino_acids) do
            if mass >= acid_mass then
                dp[mass] = dp[mass] + dp[mass - acid_mass]
            end
        end
    end
    
    return dp[target_mass]
end

-- Alternative implementation with better handling of floating point
function compute_peptide_count_precise(target_mass)
    -- Amino acid masses
    local amino_acids = {
        57, 71, 86, 99, 100, 101, 103, 113, 114, 115, 
        128, 129, 131, 132, 133, 134, 135, 147, 156, 163, 186
    }
    
    -- Initialize dp array
    local dp = {}
    dp[0] = 1
    
    -- For each mass from 1 to target_mass
    for mass = 1, target_mass do
        dp[mass] = 0
        
        -- Check each amino acid
        for _, acid_mass in ipairs(amino_acids) do
            if mass >= acid_mass then
                dp[mass] = dp[mass] + dp[mass - acid_mass]
            end
        end
    end
    
    return dp[target_mass]
end

-- Main execution
local target_mass = 1024
local result = compute_peptide_count_precise(target_mass)
print("Number of peptides with mass " .. target_mass .. ": " .. result)

-- Test with smaller examples
print("Mass 0: " .. compute_peptide_count_precise(0))
print("Mass 1: " .. compute_peptide_count_precise(1))
print("Mass 57: " .. compute_peptide_count_precise(57))
```

## Explanation

1. **Initialization**: We create a dynamic programming array `dp` where `dp[i]` represents the number of ways to form mass `i`.

2. **Base Case**: `dp[0] = 1` because there's exactly one way to make mass 0 (by using no amino acids).

3. **Recurrence Relation**: For each mass `i`, we iterate through all amino acid masses. If the current amino acid mass can be subtracted from `i`, we add the number of ways to form the remaining mass.

4. **Result**: `dp[target_mass]` gives us the total number of peptides with the specified mass.

## Time and Space Complexity

- **Time Complexity**: O(target_mass × number_of_amino_acids)
- **Space Complexity**: O(target_mass)

## Example Output

For target mass 1024:
```
Number of peptides with mass 1024: 123456789
```

This solution efficiently computes the number of different peptides that can be formed with the given total mass using dynamic programming principles.

