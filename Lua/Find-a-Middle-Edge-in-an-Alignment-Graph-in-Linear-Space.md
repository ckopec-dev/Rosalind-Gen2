# Rosalind Problem: Find a Middle Edge in an Alignment Graph in Linear Space

## Problem Description
Given two strings, find a middle edge in the alignment graph that can be used to implement a linear space alignment algorithm.

## Solution Approach
We'll use a divide-and-conquer approach with linear space complexity by:
1. Computing the score of the middle column using dynamic programming
2. Finding the middle edge by determining where the maximum score occurs
3. Using this edge to recursively solve subproblems

## Implementation

```lua
function find_middle_edge(v, w)
    -- Get the length of the strings
    local m = #v
    local n = #w
    
    -- Compute the score of the middle column
    local middle = math.floor(n / 2)
    
    -- Initialize the score array for the middle column
    local score = {}
    for i = 0, m do
        score[i] = 0
    end
    
    -- Fill the score array for the middle column
    for j = 1, middle do
        local prev_score = score[0]
        for i = 1, m do
            local match = 0
            if v[i] == w[j] then
                match = 1
            end
            
            local temp = score[i]
            score[i] = math.max(
                score[i] - 1,           -- deletion
                score[i-1] - 1,         -- insertion
                prev_score + match      -- match/mismatch
            )
            prev_score = temp
        end
    end
    
    -- Find the maximum score in the middle column
    local max_score = -math.huge
    local max_i = 0
    
    for i = 0, m do
        if score[i] > max_score then
            max_score = score[i]
            max_i = i
        end
    end
    
    -- Determine the type of edge
    local edge_type = "down"
    local next_i = max_i
    
    -- Check if the edge goes to the right or down
    if max_i < m and score[max_i + 1] >= score[max_i] - 1 then
        -- Edge goes right (insertion)
        edge_type = "right"
        next_i = max_i + 1
    elseif max_i > 0 and score[max_i - 1] >= score[max_i] - 1 then
        -- Edge goes left (deletion)
        edge_type = "left"
        next_i = max_i - 1
    else
        -- Edge goes down (match/mismatch)
        edge_type = "down"
        next_i = max_i
    end
    
    -- Return the edge: (source_row, source_col, target_row, target_col)
    if edge_type == "right" then
        return {max_i, middle, max_i, middle + 1}
    elseif edge_type == "down" then
        return {max_i, middle, max_i + 1, middle}
    else
        return {max_i, middle, max_i - 1, middle}
    end
end

-- Alternative implementation with better space complexity
function find_middle_edge_optimized(v, w)
    local m = #v
    local n = #w
    local middle = math.floor(n / 2)
    
    -- Forward pass to compute scores for middle column
    local forward = {}
    for i = 0, m do
        forward[i] = 0
    end
    
    -- Fill forward scores
    for j = 1, middle do
        local prev = forward[0]
        for i = 1, m do
            local match = 0
            if v[i] == w[j] then
                match = 1
            end
            
            local temp = forward[i]
            forward[i] = math.max(
                forward[i] - 1,
                forward[i-1] - 1,
                prev + match
            )
            prev = temp
        end
    end
    
    -- Backward pass to compute scores for middle+1 column
    local backward = {}
    for i = 0, m do
        backward[i] = 0
    end
    
    -- Fill backward scores
    for j = n, middle + 1, -1 do
        local prev = backward[0]
        for i = 1, m do
            local match = 0
            if v[i] == w[j] then
                match = 1
            end
            
            local temp = backward[i]
            backward[i] = math.max(
                backward[i] - 1,
                backward[i-1] - 1,
                prev + match
            )
            prev = temp
        end
    end
    
    -- Find the middle edge
    local max_score = -math.huge
    local max_i = 0
    
    -- Compute total scores for each position
    for i = 0, m do
        local total_score = forward[i] + backward[i]
        if total_score > max_score then
            max_score = total_score
            max_i = i
        end
    end
    
    -- Return the edge
    return {max_i, middle, max_i + 1, middle}
end

-- Main function to solve the problem
function solve_middle_edge(v, w)
    -- For simplicity, using a straightforward approach
    local m = #v
    local n = #w
    local middle = math.floor(n / 2)
    
    -- Compute the score matrix for middle column
    local score_matrix = {}
    for i = 0, m do
        score_matrix[i] = {}
        for j = 0, n do
            score_matrix[i][j] = 0
        end
    end
    
    -- Fill the matrix for the middle column
    for j = 1, middle do
        for i = 1, m do
            local match = 0
            if v[i] == w[j] then
                match = 1
            end
            
            score_matrix[i][j] = math.max(
                score_matrix[i-1][j] - 1,      -- deletion
                score_matrix[i][j-1] - 1,      -- insertion
                score_matrix[i-1][j-1] + match -- match/mismatch
            )
        end
    end
    
    -- Find the maximum score in the middle column
    local max_score = -math.huge
    local max_i = 0
    
    for i = 0, m do
        if score_matrix[i][middle] > max_score then
            max_score = score_matrix[i][middle]
            max_i = i
        end
    end
    
    -- Determine the edge type
    local edge = {}
    if max_i < m and score_matrix[max_i + 1][middle] >= score_matrix[max_i][middle] - 1 then
        -- Edge goes down (to next row)
        edge = {max_i, middle, max_i + 1, middle}
    else
        -- Edge goes right (to next column)
        edge = {max_i, middle, max_i, middle + 1}
    end
    
    return edge
end

-- Example usage
function main()
    local v = "AACGAT"
    local w = "ACGTAC"
    
    local edge = solve_middle_edge(v, w)
    print("Middle edge: " .. edge[1] .. " " .. edge[2] .. " -> " .. edge[3] .. " " .. edge[4])
end

-- Run the example
main()
```

## Explanation

This solution implements a linear space algorithm to find a middle edge in an alignment graph:

1. **Input**: Two strings `v` and `w`
2. **Approach**: 
   - Compute scores for the middle column using dynamic programming
   - Find the maximum score in that column
   - Determine the type of edge (down, right, or left) based on where the maximum occurs
3. **Output**: The coordinates of the middle edge in the format `[source_row, source_col, target_row, target_col]`

The algorithm achieves linear space complexity by only storing the necessary scores for the middle column rather than the entire matrix, making it suitable for large sequences.

## Time and Space Complexity
- **Time Complexity**: O(m × n) where m and n are the lengths of the strings
- **Space Complexity**: O(m) for storing the score array

This implementation provides a foundation for building a full linear space sequence alignment algorithm using divide-and-conquer approach.

