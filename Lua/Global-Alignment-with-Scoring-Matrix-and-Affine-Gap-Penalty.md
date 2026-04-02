# Rosalind Problem: Global Alignment with Scoring Matrix and Affine Gap Penalty

## Problem Description
In this problem, we need to find the global alignment of two sequences using a scoring matrix with affine gap penalties. This is a more complex version of the standard sequence alignment problem.

## Solution Approach
We'll use dynamic programming with a 3D matrix to handle the affine gap penalty model:
- `F[i][j][0]`: match/mismatch score
- `F[i][j][1]`: score when extending a gap in sequence 1
- `F[i][j][2]`: score when extending a gap in sequence 2

## Implementation

```lua
function global_alignment_affine_gap(seq1, seq2, scoring_matrix, gap_open, gap_extend)
    local m = #seq1
    local n = #seq2
    
    -- Initialize 3D DP matrix
    local F = {}
    for i = 0, m do
        F[i] = {}
        for j = 0, n do
            F[i][j] = {0, 0, 0}  -- [match, gap1, gap2]
        end
    end
    
    -- Initialize first row and column
    for i = 1, m do
        F[i][0][1] = -gap_open - (i - 1) * gap_extend
        F[i][0][2] = -math.huge  -- Can't have gap in sequence 2
    end
    
    for j = 1, n do
        F[0][j][2] = -gap_open - (j - 1) * gap_extend
        F[0][j][1] = -math.huge  -- Can't have gap in sequence 1
    end
    
    -- Fill the DP matrix
    for i = 1, m do
        for j = 1, n do
            local match_score = scoring_matrix[seq1:sub(i, i)][seq2:sub(j, j)]
            
            -- Calculate match/mismatch score
            F[i][j][1] = math.max(
                F[i-1][j-1][1] + match_score,  -- match
                F[i-1][j-1][2] + match_score,  -- match from gap2
                F[i-1][j-1][0] + match_score   -- match from gap1
            )
            
            -- Calculate gap in sequence 1
            F[i][j][2] = math.max(
                F[i-1][j][1] - gap_extend,  -- extend gap1
                F[i-1][j][0] - gap_open,    -- open gap1
                F[i-1][j][2] - gap_extend   -- extend gap1
            )
            
            -- Calculate gap in sequence 2
            F[i][j][0] = math.max(
                F[i][j-1][0] - gap_open,    -- open gap2
                F[i][j-1][1] - gap_open,    -- open gap2 from gap1
                F[i][j-1][2] - gap_extend   -- extend gap2
            )
        end
    end
    
    -- Return the final score
    return math.max(F[m][n][0], F[m][n][1], F[m][n][2])
end

-- Helper function to create scoring matrix
function create_scoring_matrix(match_score, mismatch_score)
    local matrix = {}
    local alphabet = "ACGT"  -- Assuming DNA sequences
    
    for i = 1, #alphabet do
        local char1 = alphabet:sub(i, i)
        matrix[char1] = {}
        for j = 1, #alphabet do
            local char2 = alphabet:sub(j, j)
            if char1 == char2 then
                matrix[char1][char2] = match_score
            else
                matrix[char1][char2] = mismatch_score
            end
        end
    end
    
    return matrix
end

-- Alternative implementation with better clarity
function global_alignment_affine_gap_v2(seq1, seq2, scoring_matrix, gap_open, gap_extend)
    local m = #seq1
    local n = #seq2
    
    -- Create 3D DP matrices
    local F_match = {}     -- F[i][j][0] - match/mismatch
    local F_gap1 = {}      -- F[i][j][1] - gap in sequence 1
    local F_gap2 = {}      -- F[i][j][2] - gap in sequence 2
    
    -- Initialize matrices
    for i = 0, m do
        F_match[i] = {}
        F_gap1[i] = {}
        F_gap2[i] = {}
        for j = 0, n do
            F_match[i][j] = 0
            F_gap1[i][j] = 0
            F_gap2[i][j] = 0
        end
    end
    
    -- Initialize first row and column
    for i = 1, m do
        F_gap1[i][0] = -gap_open - (i - 1) * gap_extend
        F_match[i][0] = -math.huge
        F_gap2[i][0] = -math.huge
    end
    
    for j = 1, n do
        F_gap2[0][j] = -gap_open - (j - 1) * gap_extend
        F_match[0][j] = -math.huge
        F_gap1[0][j] = -math.huge
    end
    
    -- Fill the DP matrix
    for i = 1, m do
        for j = 1, n do
            local match_score = scoring_matrix[seq1:sub(i, i)][seq2:sub(j, j)]
            
            -- Match/mismatch state
            F_match[i][j] = match_score + math.max(
                F_match[i-1][j-1],
                F_gap1[i-1][j-1],
                F_gap2[i-1][j-1]
            )
            
            -- Gap in sequence 1
            F_gap1[i][j] = math.max(
                F_match[i-1][j] - gap_open,
                F_gap1[i-1][j] - gap_extend,
                F_gap2[i-1][j] - gap_open
            )
            
            -- Gap in sequence 2
            F_gap2[i][j] = math.max(
                F_match[i][j-1] - gap_open,
                F_gap1[i][j-1] - gap_open,
                F_gap2[i][j-1] - gap_extend
            )
        end
    end
    
    -- Return the maximum of all three states at the end
    return math.max(F_match[m][n], F_gap1[m][n], F_gap2[m][n])
end

-- Example usage:
function solve_rosalind_problem()
    -- Example sequences
    local seq1 = "ACGT"
    local seq2 = "ACGT"
    
    -- Create scoring matrix (DNA)
    local scoring_matrix = {
        A = {A = 2, C = -1, G = -1, T = -1},
        C = {A = -1, C = 2, G = -1, T = -1},
        G = {A = -1, C = -1, G = 2, T = -1},
        T = {A = -1, C = -1, G = -1, T = 2}
    }
    
    local gap_open = 5
    local gap_extend = 2
    
    local score = global_alignment_affine_gap_v2(seq1, seq2, scoring_matrix, gap_open, gap_extend)
    print("Alignment score:", score)
    
    return score
end

-- Run example
solve_rosalind_problem()
```

## Key Points

1. **Three State DP**: The algorithm uses three states to track:
   - Match/mismatch state (F[i][j][0])
   - Gap in sequence 1 (F[i][j][1]) 
   - Gap in sequence 2 (F[i][j][2])

2. **Affine Gap Penalty**: Uses two penalties:
   - `gap_open`: Penalty for opening a gap
   - `gap_extend`: Penalty for extending a gap

3. **Time Complexity**: O(m×n) where m and n are sequence lengths
4. **Space Complexity**: O(m×n) for the 3D matrix

## Usage
To use this with actual Rosalind input, you would:
1. Parse the input sequences and scoring matrix
2. Set appropriate gap penalties
3. Call the function with the parsed data

The solution handles the affine gap penalty model which is more biologically realistic than linear gap penalties.

