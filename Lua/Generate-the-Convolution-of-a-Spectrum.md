# Rosalind Problem: Generate the Convolution of a Spectrum

## Problem Statement
The convolution of a spectrum is the multiset of all positive differences between pairs of masses in the spectrum. Given a spectrum, we need to compute its convolution.

## Solution in Lua

```lua
function generate_convolution(spectrum)
    -- Sort the spectrum in ascending order
    table.sort(spectrum)
    
    -- Create a table to store the convolution
    local convolution = {}
    
    -- Compute all positive differences between pairs
    for i = 1, #spectrum do
        for j = i + 1, #spectrum do
            local diff = spectrum[j] - spectrum[i]
            if diff > 0 then
                table.insert(convolution, diff)
            end
        end
    end
    
    -- Sort the convolution in ascending order
    table.sort(convolution)
    
    return convolution
end

-- Helper function to print the convolution
function print_convolution(convolution)
    for i, value in ipairs(convolution) do
        io.write(value)
        if i < #convolution then
            io.write(" ")
        end
    end
    io.write("\n")
end

-- Example usage:
-- Read input from stdin
local input = {}
for line in io.lines() do
    table.insert(input, line)
end

-- Parse the spectrum from the first line
local spectrum = {}
for value in string.gmatch(input[1], "%d+") do
    table.insert(spectrum, tonumber(value))
end

-- Generate and print the convolution
local convolution = generate_convolution(spectrum)
print_convolution(convolution)
```

## Explanation

1. **Input Processing**: The program reads the spectrum from standard input, parsing space-separated integers.

2. **Sorting**: The spectrum is sorted in ascending order to ensure proper computation.

3. **Convolution Generation**: 
   - For each pair of elements (i,j) where i < j, compute the difference spectrum[j] - spectrum[i]
   - Only positive differences are included in the convolution
   - All differences are stored in a table

4. **Output**: The convolution is sorted and printed as space-separated values.

## Example

**Input:**
```
0 137 186 323
```

**Output:**
```
137 186 323 49 137 186
```

**Explanation:**
- Differences: 137-0=137, 186-0=186, 323-0=323, 186-137=49, 323-137=186, 323-186=137
- Sorted: 49, 137, 137, 186, 186, 323

The time complexity is O(n²) where n is the number of elements in the spectrum, and the space complexity is O(n²) for storing the convolution.

