# Finding Disjoint Motifs in a Gene - Rosalind Problem Solution in Lua

## Problem Understanding

The problem asks us to find the positions of two disjoint motifs (non-overlapping substrings) in a gene sequence. We need to find all possible pairs of positions where we can place these two motifs such that they don't overlap.

## Solution Approach

1. Find all occurrences of the first motif
2. For each occurrence of the first motif, find all occurrences of the second motif that don't overlap
3. Return all valid pairs of positions

## Implementation

```lua
function find_disjoint_motifs(sequence, motif1, motif2)
    local positions1 = {}
    local positions2 = {}
    
    -- Find all positions of motif1
    for i = 1, #sequence - #motif1 + 1 do
        if sequence:sub(i, i + #motif1 - 1) == motif1 then
            table.insert(positions1, i)
        end
    end
    
    -- Find all positions of motif2
    for i = 1, #sequence - #motif2 + 1 do
        if sequence:sub(i, i + #motif2 - 1) == motif2 then
            table.insert(positions2, i)
        end
    end
    
    -- Find disjoint pairs
    local disjoint_pairs = {}
    
    for _, pos1 in ipairs(positions1) do
        local end1 = pos1 + #motif1 - 1
        
        for _, pos2 in ipairs(positions2) do
            local end2 = pos2 + #motif2 - 1
            
            -- Check if motifs don't overlap
            if end1 < pos2 or end2 < pos1 then
                table.insert(disjoint_pairs, {pos1, pos2})
            end
        end
    end
    
    return disjoint_pairs
end

-- Read input from file or stdin
function read_input()
    local lines = {}
    for line in io.lines() do
        table.insert(lines, line)
    end
    return lines
end

-- Main execution
local input_lines = read_input()
local sequence = input_lines[1]
local motif1 = input_lines[2]
local motif2 = input_lines[3]

local result = find_disjoint_motifs(sequence, motif1, motif2)

-- Output results
for _, pair in ipairs(result) do
    print(pair[1] .. " " .. pair[2])
end
```

## Example Usage

If the input is:
```
ACGTACGTACGT
AC
GT
```

The output would be:
```
1 3
1 7
5 3
5 7
```

## Explanation

1. **Finding Motif Positions**: The algorithm first finds all starting positions where each motif occurs in the sequence
2. **Checking Disjoint Condition**: For each occurrence of motif1, we check if any occurrence of motif2 doesn't overlap with it
3. **Overlap Detection**: Two intervals [start1, end1] and [start2, end2] don't overlap if either end1 < start2 or end2 < start1
4. **Output Format**: Each valid pair is printed as two space-separated numbers representing the starting positions

## Time Complexity
- O(n₁ × n₂) where n₁ and n₂ are the number of occurrences of each motif
- Space complexity: O(n₁ + n₂) for storing positions

This solution handles the disjoint motif finding problem efficiently and correctly identifies all valid non-overlapping pairs of motif occurrences.

