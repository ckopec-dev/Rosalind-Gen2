# Finding a Spliced Motif - Rosalind Problem Solution in Lua

## Problem Understanding

The problem asks us to find a subsequence (spliced motif) of a given string that matches a pattern string. Unlike finding a substring, a subsequence maintains the relative order of characters but doesn't need to be contiguous.

## Solution Approach

We'll use dynamic programming to solve this problem:
1. Create a 2D table to track matching positions
2. Fill the table to find where pattern characters appear in the string
3. Trace back to find the actual subsequence positions

## Implementation

```lua
function find_spliced_motif(s, t)
    local m = #s
    local n = #t
    
    -- Create a 2D table to store matching positions
    local dp = {}
    for i = 0, m do
        dp[i] = {}
        for j = 0, n do
            dp[i][j] = 0
        end
    end
    
    -- Fill the dp table
    for i = 1, m do
        for j = 1, n do
            if s:sub(i, i) == t:sub(j, j) then
                dp[i][j] = dp[i-1][j-1] + 1
            else
                dp[i][j] = math.max(dp[i-1][j], dp[i][j-1])
            end
        end
    end
    
    -- If no match found, return empty
    if dp[m][n] == 0 then
        return ""
    end
    
    -- Trace back to find the actual subsequence
    local result = {}
    local i, j = m, n
    
    while i > 0 and j > 0 do
        if s:sub(i, i) == t:sub(j, j) then
            table.insert(result, 1, i)  -- Store 1-based position
            i = i - 1
            j = j - 1
        elseif dp[i-1][j] > dp[i][j-1] then
            i = i - 1
        else
            j = j - 1
        end
    end
    
    -- Convert to space-separated string
    local output = ""
    for k, pos in ipairs(result) do
        if k > 1 then output = output .. " " end
        output = output .. pos
    end
    
    return output
end

-- Read input from stdin
local lines = {}
for line in io.lines() do
    table.insert(lines, line)
end

-- Parse input (assuming FASTA format with two sequences)
local s = lines[2]  -- First sequence
local t = lines[4]  -- Second sequence (motif)

-- Solve and print result
print(find_spliced_motif(s, t))
```

## Alternative Simpler Approach

Since we want to find the actual positions of a subsequence, here's a cleaner approach:

```lua
function find_spliced_motif_simple(s, t)
    local positions = {}
    local j = 1  -- Position in pattern t
    
    -- Find each character of t in s in order
    for i = 1, #s do
        if j <= #t and s:sub(i, i) == t:sub(j, j) then
            table.insert(positions, i)
            j = j + 1
        end
    end
    
    -- Check if we found all characters
    if #positions == #t then
        -- Convert to space-separated string
        local result = ""
        for i, pos in ipairs(positions) do
            if i > 1 then result = result .. " " end
            result = result .. pos
        end
        return result
    else
        return ""  -- Not found
    end
end

-- Read input
local lines = {}
for line in io.lines() do
    table.insert(lines, line)
end

local s = lines[2]
local t = lines[4]

print(find_spliced_motif_simple(s, t))
```

## Example Usage

For input:
```
>Rosalind_3
ACGTACGTGACG
>Rosalind_4
TGC
```

The output would be:
```
3 6 9
```

This represents the positions (1-indexed) where characters of the motif "TGC" appear in the sequence in the correct order.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the dynamic programming table

The simpler approach has O(m + n) time complexity and O(n) space complexity, which is more efficient for this specific problem.

