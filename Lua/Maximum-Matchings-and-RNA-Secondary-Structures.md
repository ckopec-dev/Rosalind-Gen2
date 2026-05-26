# Rosalind Problem: Maximum Matchings and RNA Secondary Structures

## Problem Understanding

This problem asks us to find the maximum number of base pairings in an RNA sequence such that:
1. Base pairs follow Watson-Crick rules (A-U, U-A, G-C, C-G)
2. No base pairs cross each other (non-crossing)
3. We want to maximize the number of base pairs

This is essentially finding the maximum matching in a RNA secondary structure, which can be solved using dynamic programming.

## Solution Approach

We'll use dynamic programming where `dp[i][j]` represents the maximum number of base pairs in the substring from index `i` to `j`.

## Implementation

```lua
function max_matchings(rna)
    local n = #rna
    if n <= 1 then return 0 end
    
    -- Create a function to check if two nucleotides can pair
    local function can_pair(nuc1, nuc2)
        local pairs = {
            ['A'] = 'U',
            ['U'] = 'A',
            ['G'] = 'C',
            ['C'] = 'G'
        }
        return pairs[nuc1] == nuc2
    end
    
    -- Create DP table
    local dp = {}
    for i = 1, n do
        dp[i] = {}
        for j = 1, n do
            dp[i][j] = 0
        end
    end
    
    -- Fill the DP table
    for l = 2, n do  -- l is the length of the substring
        for i = 1, n - l + 1 do
            local j = i + l - 1
            
            -- Case 1: No pairing at position j
            dp[i][j] = dp[i][j-1]
            
            -- Case 2: Pairing at position j with some position k
            for k = i, j - 1 do
                if can_pair(rna:sub(k, k), rna:sub(j, j)) then
                    local score = 1
                    if k > i then
                        score = score + dp[i][k-1]
                    end
                    if j > k + 1 then
                        score = score + dp[k+1][j-1]
                    end
                    dp[i][j] = math.max(dp[i][j], score)
                end
            end
        end
    end
    
    return dp[1][n]
end

-- Alternative simpler approach for this specific problem
function max_matchings_simple(rna)
    local n = #rna
    if n <= 1 then return 0 end
    
    -- Create a function to check if two nucleotides can pair
    local function can_pair(nuc1, nuc2)
        local pairs = {
            ['A'] = 'U',
            ['U'] = 'A',
            ['G'] = 'C',
            ['C'] = 'G'
        }
        return pairs[nuc1] == nuc2
    end
    
    -- DP approach: dp[i] = maximum matchings for substring from 1 to i
    local dp = {}
    for i = 1, n + 1 do
        dp[i] = 0
    end
    
    -- Fill DP table
    for i = n, 1, -1 do
        -- Don't pair at position i
        dp[i] = dp[i+1]
        
        -- Try pairing with all possible positions j > i
        for j = i + 1, n do
            if can_pair(rna:sub(i, i), rna:sub(j, j)) then
                local score = 1
                if i + 1 <= j - 1 then
                    score = score + dp[j+1]  -- This is a simplified approach
                end
                dp[i] = math.max(dp[i], score)
            end
        end
    end
    
    return dp[1]
end

-- Correct approach using proper DP for non-crossing matchings
function maximum_matchings(rna)
    local n = #rna
    
    -- Function to check if two nucleotides can pair
    local function can_pair(i, j)
        local pairs = {
            ['A'] = 'U',
            ['U'] = 'A',
            ['G'] = 'C',
            ['C'] = 'G'
        }
        return pairs[rna:sub(i, i)] == rna:sub(j, j)
    end
    
    -- DP table: dp[i][j] = max matchings in substring from i to j
    local dp = {}
    for i = 1, n do
        dp[i] = {}
        for j = 1, n do
            dp[i][j] = 0
        end
    end
    
    -- Fill DP table
    for len = 2, n do  -- length of substring
        for i = 1, n - len + 1 do
            local j = i + len - 1
            
            -- Option 1: don't pair j with anything
            dp[i][j] = dp[i][j-1]
            
            -- Option 2: try pairing j with each possible i'
            for k = i, j - 1 do
                if can_pair(k, j) then
                    local val = 1
                    if k > i then
                        val = val + dp[i][k-1]
                    end
                    if j > k + 1 then
                        val = val + dp[k+1][j-1]
                    end
                    dp[i][j] = math.max(dp[i][j], val)
                end
            end
        end
    end
    
    return dp[1][n]
end

-- Read input and solve
local input = io.read()
if input then
    local result = maximum_matchings(input)
    print(result)
end
```

## Explanation

The solution uses dynamic programming to find the maximum number of non-crossing base pairs in an RNA sequence:

1. **Base Case**: For empty or single nucleotide sequences, the maximum matchings is 0
2. **Recurrence Relation**: For a substring from position `i` to `j`:
   - We can either not pair the last nucleotide `j` (value = `dp[i][j-1]`)
   - Or we can pair `j` with some nucleotide `k` (where `i ≤ k < j`) and add 1 plus the optimal solutions for the remaining segments
3. **Non-crossing Constraint**: The recurrence ensures that pairs don't cross each other

## Time and Space Complexity

- **Time Complexity**: O(n³) where n is the length of the RNA sequence
- **Space Complexity**: O(n²) for the DP table

## Example Usage

For an RNA sequence like "AUCG":
- The maximum number of non-crossing base pairs would be 2 (A-U and C-G)
- The algorithm will compute this by trying all valid pairings and selecting the maximum

