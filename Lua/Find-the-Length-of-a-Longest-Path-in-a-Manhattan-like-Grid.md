# Rosalind Problem: Find the Length of a Longest Path in a Manhattan-like Grid

## Problem Understanding

This problem asks us to find the length of the longest path in a Manhattan-like grid from the top-left corner to the bottom-right corner, where we can only move right or down.

## Approach

This is a classic dynamic programming problem. We can use a 2D DP table where `dp[i][j]` represents the maximum distance to reach cell `(i,j)` from the starting point `(0,0)`.

The recurrence relation is:
- `dp[0][0] = 0` (starting point)
- For the first row: `dp[0][j] = dp[0][j-1] + right_weights[0][j-1]`
- For the first column: `dp[i][0] = dp[i-1][0] + down_weights[i-1][0]`
- For other cells: `dp[i][j] = max(dp[i-1][j], dp[i][j-1]) + weight[i][j]`

## Solution

```lua
function longest_path(m, n, down_weights, right_weights)
    -- Create DP table
    local dp = {}
    for i = 0, m do
        dp[i] = {}
        for j = 0, n do
            dp[i][j] = 0
        end
    end
    
    -- Fill the first row (can only come from left)
    for j = 1, n do
        dp[0][j] = dp[0][j-1] + right_weights[1][j]
    end
    
    -- Fill the first column (can only come from above)
    for i = 1, m do
        dp[i][0] = dp[i-1][0] + down_weights[i][1]
    end
    
    -- Fill the rest of the table
    for i = 1, m do
        for j = 1, n do
            dp[i][j] = math.max(dp[i-1][j], dp[i][j-1]) + down_weights[i][j]
        end
    end
    
    return dp[m][n]
end

-- Read input function
function read_input()
    local input = {}
    for line in io.lines() do
        table.insert(input, line)
    end
    return input
end

-- Parse input
function parse_input(lines)
    local m, n = string.match(lines[1], "(%d+)%s+(%d+)")
    m, n = tonumber(m), tonumber(n)
    
    -- Read down weights
    local down_weights = {}
    for i = 1, m do
        down_weights[i] = {}
        local line = lines[i + 1]
        local numbers = {}
        for num in string.gmatch(line, "%S+") do
            table.insert(numbers, tonumber(num))
        end
        for j = 1, #numbers do
            down_weights[i][j] = numbers[j]
        end
    end
    
    -- Read right weights
    local right_weights = {}
    for i = 1, m + 1 do
        right_weights[i] = {}
        local line = lines[i + m + 1]
        local numbers = {}
        for num in string.gmatch(line, "%S+") do
            table.insert(numbers, tonumber(num))
        end
        for j = 1, #numbers do
            right_weights[i][j] = numbers[j]
        end
    end
    
    return m, n, down_weights, right_weights
end

-- Main execution
local lines = read_input()
local m, n, down_weights, right_weights = parse_input(lines)

-- Calculate and print result
local result = longest_path(m, n, down_weights, right_weights)
print(result)
```

## Example Usage

For input like:
```
4 4
1 0 2 4 3
4 6 5 2 1
4 4 5 2 1
5 6 8 1 2
3 2 4 1
3 2 4 1
3 2 4 1
3 2 4 1
```

The solution will compute the longest path from top-left to bottom-right.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the dimensions of the grid
- **Space Complexity**: O(m × n) for the DP table

## Key Insights

1. This is a classic dynamic programming problem with overlapping subproblems
2. At each cell, we can only come from either the cell above or the cell to the left
3. We need to carefully handle the boundary conditions for the first row and first column
4. The Manhattan grid structure ensures we only move right or down, making the solution straightforward

