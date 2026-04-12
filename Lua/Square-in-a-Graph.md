# Rosalind Problem: Square_in_a_Graph

## Problem Understanding

A square in a graph is a cycle of length 4 (a 4-cycle). Given a graph, we need to determine if it contains a square.

## Approach

1. **Graph Representation**: Use an adjacency list representation
2. **Cycle Detection**: For each pair of vertices, check if they form a 4-cycle
3. **Brute Force**: Check all possible 4-cycles by examining all combinations of 4 vertices

## Solution

```lua
-- Function to check if a graph contains a square (4-cycle)
function has_square(graph)
    local vertices = {}
    for v in pairs(graph) do
        table.insert(vertices, v)
    end
    
    local n = #vertices
    
    -- Check all combinations of 4 vertices
    for i = 1, n do
        for j = i + 1, n do
            for k = j + 1, n do
                for l = k + 1, n do
                    local v1, v2, v3, v4 = vertices[i], vertices[j], vertices[k], vertices[l]
                    
                    -- Check if these 4 vertices form a square
                    if is_square(graph, v1, v2, v3, v4) then
                        return true
                    end
                end
            end
        end
    end
    
    return false
end

-- Function to check if 4 vertices form a square
function is_square(graph, v1, v2, v3, v4)
    -- A square has 4 edges connecting vertices in a cycle
    -- Check if there's a cycle v1-v2-v3-v4-v1
    
    -- Check if all edges exist
    local edges = {
        {v1, v2}, {v2, v3}, {v3, v4}, {v4, v1}
    }
    
    for _, edge in ipairs(edges) do
        local u, v = edge[1], edge[2]
        if not graph[u] or not graph[u][v] then
            return false
        end
    end
    
    return true
end

-- Alternative approach: More efficient for checking 4-cycles
function has_square_efficient(graph)
    local vertices = {}
    for v in pairs(graph) do
        table.insert(vertices, v)
    end
    
    local n = #vertices
    
    -- For each vertex, check all pairs of its neighbors
    for _, v1 in ipairs(vertices) do
        local neighbors1 = {}
        for v in pairs(graph[v1]) do
            table.insert(neighbors1, v)
        end
        
        -- Check pairs of neighbors
        for i = 1, #neighbors1 do
            for j = i + 1, #neighbors1 do
                local v2 = neighbors1[i]
                local v3 = neighbors1[j]
                
                -- Check if v2 and v3 are connected
                if graph[v2] and graph[v2][v3] then
                    -- Found a triangle, now check if we can extend to a square
                    -- We need to find a fourth vertex connected to v1 and v3
                    for _, v4 in ipairs(vertices) do
                        if v4 ~= v1 and v4 ~= v2 and v4 ~= v3 then
                            -- Check if v4 is connected to both v1 and v3
                            if graph[v1] and graph[v1][v4] and graph[v3] and graph[v3][v4] then
                                return true
                            end
                        end
                    end
                end
            end
        end
    end
    
    return false
end

-- Parse input from file or string
function parse_graph(input)
    local graph = {}
    
    for line in input:gmatch("[^\r\n]+") do
        if line:match("^%d+%s+%d+") then
            -- This is a vertex pair
            local u, v = line:match("(%d+)%s+(%d+)")
            u, v = tonumber(u), tonumber(v)
            
            if not graph[u] then graph[u] = {} end
            if not graph[v] then graph[v] = {} end
            
            graph[u][v] = true
            graph[v][u] = true
        end
    end
    
    return graph
end

-- Main function to solve the problem
function solve_square_in_graph(input)
    local graph = parse_graph(input)
    return has_square_efficient(graph)
end

-- Example usage
local input_example = [[3 2
1 2
2 3
3 1]]

-- Parse and solve
local result = solve_square_in_graph(input_example)
print("Has square:", result)

-- More complex example
local input_complex = [[5 6
1 2
2 3
3 4
4 5
5 1
1 3]]

local result2 = solve_square_in_graph(input_complex)
print("Has square (complex):", result2)
```

## Explanation

This solution implements two approaches:

1. **Brute Force Approach**: Checks all combinations of 4 vertices to see if they form a square
2. **Efficient Approach**: For each vertex, examines pairs of its neighbors to find potential 4-cycles

The key insight is that a square is a 4-cycle, so we need to find 4 vertices where each consecutive pair is connected, and the first and last are also connected.

## Time Complexity
- **Brute Force**: O(n⁴) where n is the number of vertices
- **Efficient**: O(n³) in worst case but better in practice

## Space Complexity
O(n + m) where n is the number of vertices and m is the number of edges

The efficient approach is preferred for larger graphs as it reduces unnecessary checks by leveraging the structure of the graph.

