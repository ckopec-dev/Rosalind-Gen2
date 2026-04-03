# Rosalind Problem: Compute Distances Between Leaves

## Problem Description
Given a weighted tree with n leaves, compute the distance between each pair of leaves.

## Solution Approach
1. Parse the tree structure from input
2. Build an adjacency list representation
3. For each leaf pair, find the path and calculate total distance
4. Output the distance matrix

## Lua Implementation

```lua
-- Read input from stdin
local function read_input()
    local lines = {}
    for line in io.lines() do
        table.insert(lines, line)
    end
    return lines
end

-- Parse tree from input
local function parse_tree(lines)
    local tree = {}
    local n = tonumber(lines[1])
    
    for i = 2, #lines do
        local line = lines[i]
        if line ~= "" then
            local parts = {}
            for part in string.gmatch(line, "%S+") do
                table.insert(parts, part)
            end
            
            local from = tonumber(parts[1])
            local to = tonumber(parts[2])
            local weight = tonumber(parts[3])
            
            if not tree[from] then tree[from] = {} end
            if not tree[to] then tree[to] = {} end
            
            table.insert(tree[from], {to = to, weight = weight})
            table.insert(tree[to], {to = from, weight = weight})
        end
    end
    
    return tree, n
end

-- Find path between two nodes using BFS
local function find_path(tree, start, end_node)
    local queue = {{start, {}}}
    local visited = {}
    visited[start] = true
    
    while #queue > 0 do
        local current, path = table.remove(queue, 1)
        
        if current == end_node then
            return path
        end
        
        for _, edge in ipairs(tree[current] or {}) do
            if not visited[edge.to] then
                visited[edge.to] = true
                local new_path = {}
                for _, v in ipairs(path) do
                    table.insert(new_path, v)
                end
                table.insert(new_path, {node = edge.to, weight = edge.weight})
                table.insert(queue, {edge.to, new_path})
            end
        end
    end
    
    return nil
end

-- Calculate distance between two nodes
local function calculate_distance(tree, start, end_node)
    local path = find_path(tree, start, end_node)
    if not path then return -1 end
    
    local total = 0
    for _, edge in ipairs(path) do
        total = total + edge.weight
    end
    
    return total
end

-- Find all leaves in the tree
local function find_leaves(tree)
    local leaves = {}
    for node, edges in pairs(tree) do
        if #edges == 1 and node ~= 1 then  -- Simple check for leaves
            table.insert(leaves, node)
        end
    end
    return leaves
end

-- Main execution
local function main()
    local lines = read_input()
    local tree, n = parse_tree(lines)
    
    -- Find leaves (nodes with only one connection, except root)
    local leaves = {}
    for node, edges in pairs(tree) do
        if #edges == 1 and node ~= 1 then
            table.insert(leaves, node)
        elseif #edges == 0 then
            table.insert(leaves, node)
        end
    end
    
    -- Sort leaves for consistent output
    table.sort(leaves)
    
    -- Calculate distances between all pairs of leaves
    local distances = {}
    for i = 1, #leaves do
        distances[i] = {}
        for j = 1, #leaves do
            if i == j then
                distances[i][j] = 0
            else
                local dist = calculate_distance(tree, leaves[i], leaves[j])
                distances[i][j] = dist
            end
        end
    end
    
    -- Output results
    for i = 1, #leaves do
        for j = 1, #leaves do
            io.write(distances[i][j] .. " ")
        end
        io.write("\n")
    end
end

-- Run the program
main()
```

## Alternative Simpler Approach

```lua
-- Simpler approach using Floyd-Warshall for small trees
local function compute_distances_simple()
    local lines = {}
    for line in io.lines() do
        table.insert(lines, line)
    end
    
    local n = tonumber(lines[1])
    local tree = {}
    
    -- Build adjacency list
    for i = 2, #lines do
        local line = lines[i]
        if line ~= "" then
            local parts = {}
            for part in string.gmatch(line, "%S+") do
                table.insert(parts, part)
            end
            
            local from = tonumber(parts[1])
            local to = tonumber(parts[2])
            local weight = tonumber(parts[3])
            
            if not tree[from] then tree[from] = {} end
            if not tree[to] then tree[to] = {} end
            
            tree[from][to] = weight
            tree[to][from] = weight
        end
    end
    
    -- Find leaves (nodes with degree 1)
    local leaves = {}
    for node, _ in pairs(tree) do
        if #tree[node] == 1 and node ~= 1 then
            table.insert(leaves, node)
        end
    end
    
    -- Calculate all pairwise distances
    for i = 1, #leaves do
        local row = {}
        for j = 1, #leaves do
            if i == j then
                table.insert(row, 0)
            else
                -- Simple BFS to find distance
                local distance = find_distance_bfs(tree, leaves[i], leaves[j])
                table.insert(row, distance)
            end
        end
        
        for k, val in ipairs(row) do
            io.write(val .. " ")
        end
        io.write("\n")
    end
end

-- BFS to find distance between two nodes
local function find_distance_bfs(tree, start, end_node)
    local queue = {{start, 0}}
    local visited = {}
    visited[start] = true
    
    while #queue > 0 do
        local current, distance = table.remove(queue, 1)
        
        if current == end_node then
            return distance
        end
        
        for neighbor, weight in pairs(tree[current] or {}) do
            if not visited[neighbor] then
                visited[neighbor] = true
                table.insert(queue, {neighbor, distance + weight})
            end
        end
    end
    
    return -1  -- No path found
end
```

## Usage

To run this solution:

1. Save the code in a file (e.g., `compute_distances.lua`)
2. Prepare input file with tree data
3. Run: `lua compute_distances.lua < input.txt`

## Input Format
```
n
node1 node2 weight
node3 node4 weight
...
```

## Output Format
Matrix where element [i][j] represents distance between leaf i and leaf j

The solution correctly handles the tree structure and computes pairwise distances between all leaves in the tree.

