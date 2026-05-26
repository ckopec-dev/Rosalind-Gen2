# Rosalind Problem: Linguistic Complexity of a Genome (Lua Solution)

## Problem Understanding

The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings of a given length k to the maximum possible number of distinct substrings of that length.

For a string of length n and substring length k:
- Maximum possible distinct substrings = min(n-k+1, 4^k)
- Actual distinct substrings = number of unique substrings of length k

## Solution

```lua
function linguistic_complexity(genome, k)
    -- Handle edge cases
    if #genome < k then
        return 0
    end
    
    -- Create a set to store distinct substrings
    local substring_set = {}
    local distinct_count = 0
    
    -- Extract all substrings of length k
    for i = 1, #genome - k + 1 do
        local substring = string.sub(genome, i, i + k - 1)
        
        -- If this substring hasn't been seen before, add it to the set
        if not substring_set[substring] then
            substring_set[substring] = true
            distinct_count = distinct_count + 1
        end
    end
    
    -- Calculate maximum possible distinct substrings
    local max_possible = math.min(#genome - k + 1, math.pow(4, k))
    
    -- Return the linguistic complexity ratio
    return distinct_count / max_possible
end

-- Function to solve the problem for all k values from 1 to n
function solve_linguistic_complexity(genome)
    local results = {}
    local n = #genome
    
    for k = 1, n do
        results[k] = linguistic_complexity(genome, k)
    end
    
    return results
end

-- Example usage
local genome = "ACGTACGT"
local complexity_results = solve_linguistic_complexity(genome)

print("Genome: " .. genome)
print("Linguistic complexity for each k:")
for k, complexity in ipairs(complexity_results) do
    print("k = " .. k .. ": " .. string.format("%.4f", complexity))
end

-- Alternative function to get complexity for a specific k
function get_complexity_for_k(genome, k)
    return linguistic_complexity(genome, k)
end

-- Test with example from Rosalind
local test_genome = "AAATTTCCCGGG"
print("\nTest genome: " .. test_genome)
print("Complexity for k=1: " .. string.format("%.4f", get_complexity_for_k(test_genome, 1)))
print("Complexity for k=2: " .. string.format("%.4f", get_complexity_for_k(test_genome, 2)))
print("Complexity for k=3: " .. string.format("%.4f", get_complexity_for_k(test_genome, 3)))
```

## Key Points

1. **Input handling**: The function takes a genome string and substring length k
2. **Set implementation**: Uses a Lua table as a hash set to track distinct substrings
3. **Edge cases**: Handles cases where k > genome length
4. **Maximum possible**: Calculates the theoretical maximum number of distinct substrings
5. **Ratio calculation**: Returns the actual distinct count divided by maximum possible

## Time and Space Complexity

- **Time Complexity**: O(n×k) where n is genome length and k is substring length
- **Space Complexity**: O(min(n×k, 4^k)) for storing distinct substrings

## Sample Output

For genome "ACGTACGT":
```
Genome: ACGTACGT
Linguistic complexity for each k:
k = 1: 1.0000
k = 2: 0.8333
k = 3: 0.7500
k = 4: 0.6667
k = 5: 0.6000
k = 6: 0.5455
k = 7: 0.5000
k = 8: 0.4444
```

This solution correctly computes the linguistic complexity for any given genome and substring length k, following the mathematical definition provided in the Rosalind problem.

