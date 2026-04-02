# Rosalind Problem: Double-Degree_Array

## Problem Description
Given: A simple graph with n ≤ 10³ vertices in the edge list format.
Return: An array D[1..n] where D[i] is the sum of the degrees of all neighbors of vertex i.

## Solution in Lua

```lua
-- Read input from stdin
local lines = {}
for line in io.lines() do
    table.insert(lines, line)
end

-- Parse the first line to get number of vertices and edges
local n, m = string.match(lines[1], "(%d+)%s+(%d+)")
n = tonumber(n)
m = tonumber(m)

-- Initialize adjacency list and degree array
local adj = {}
local degree = {}

-- Initialize arrays
for i = 1, n do
    adj[i] = {}
    degree[i] = 0
end

-- Process edges
for i = 2, m + 1 do
    local u, v = string.match(lines[i], "(%d+)%s+(%d+)")
    u = tonumber(u)
    v = tonumber(v)
    
    -- Add edges to adjacency list
    table.insert(adj[u], v)
    table.insert(adj[v], u)
    
    -- Increment degrees
    degree[u] = degree[u] + 1
    degree[v] = degree[v] + 1
end

-- Calculate double degrees
local double_degree = {}
for i = 1, n do
    double_degree[i] = 0
    for _, neighbor in ipairs(adj[i]) do
        double_degree[i] = double_degree[i] + degree[neighbor]
    end
end

-- Output result
local result = {}
for i = 1, n do
    table.insert(result, tostring(double_degree[i]))
end

print(table.concat(result, " "))
```

## Explanation

1. **Input parsing**: Read the graph from standard input, where the first line contains n (vertices) and m (edges), followed by m lines of edges.

2. **Data structures**: 
   - `adj[i]` stores the list of neighbors for vertex i
   - `degree[i]` stores the degree of vertex i
   - `double_degree[i]` stores the sum of degrees of all neighbors of vertex i

3. **Edge processing**: For each edge (u,v), we:
   - Add v to u's adjacency list and u to v's adjacency list
   - Increment the degree of both vertices

4. **Double degree calculation**: For each vertex i, we sum up the degrees of all its neighbors by:
   - Iterating through all neighbors of vertex i
   - Adding the degree of each neighbor to our running sum

5. **Output**: Print the double degrees separated by spaces.

## Example

**Input:**
```
5 5
1 2
2 3
3 4
4 5
5 1
```

**Output:**
```
3 4 5 4 3
```

This represents the double degrees for vertices 1 through 5, where each vertex's double degree is the sum of degrees of its neighbors.

