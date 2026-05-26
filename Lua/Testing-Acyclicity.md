# Rosalind Problem: Testing Acyclicity - Lua Solution

## Problem Understanding

The problem asks us to determine whether a given directed graph is acyclic (i.e., contains no cycles). This is equivalent to checking if the graph is a Directed Acyclic Graph (DAG).

## Approach

I'll use a topological sorting approach with Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. Process vertices from the queue, reducing in-degrees of neighbors
4. If we process all vertices, the graph is acyclic; otherwise, it contains cycles

## Solution

```lua
-- Function to test if a directed graph is acyclic
function is_acyclic(num_vertices, edges)
    -- Initialize in-degree array
    local in_degree = {}
    for i = 1, num_vertices do
        in_degree[i] = 0
    end
    
    -- Build adjacency list and calculate in-degrees
    local adj_list = {}
    for i = 1, num_vertices do
        adj_list[i] = {}
    end
    
    for _, edge in ipairs(edges) do
        local from, to = edge[1], edge[2]
        table.insert(adj_list[from], to)
        in_degree[to] = in_degree[to] + 1
    end
    
    -- Initialize queue with vertices having in-degree 0
    local queue = {}
    local queue_size = 0
    
    for i = 1, num_vertices do
        if in_degree[i] == 0 then
            queue_size = queue_size + 1
            queue[queue_size] = i
        end
    end
    
    local processed = 0
    
    -- Process vertices using BFS-like approach
    while queue_size > 0 do
        local current = queue[1]
        table.remove(queue, 1)
        queue_size = queue_size - 1
        processed = processed + 1
        
        -- Reduce in-degree of neighbors
        for _, neighbor in ipairs(adj_list[current]) do
            in_degree[neighbor] = in_degree[neighbor] - 1
            if in_degree[neighbor] == 0 then
                queue_size = queue_size + 1
                queue[queue_size] = neighbor
            end
        end
    end
    
    -- If we processed all vertices, graph is acyclic
    return processed == num_vertices
end

-- Alternative simpler approach using DFS
function is_acyclic_dfs(num_vertices, edges)
    -- Build adjacency list
    local adj_list = {}
    for i = 1, num_vertices do
        adj_list[i] = {}
    end
    
    for _, edge in ipairs(edges) do
        local from, to = edge[1], edge[2]
        table.insert(adj_list[from], to)
    end
    
    -- Track visited states: 0 = unvisited, 1 = visiting (in current path), 2 = visited
    local visited = {}
    for i = 1, num_vertices do
        visited[i] = 0
    end
    
    -- DFS helper function
    function dfs(vertex)
        if visited[vertex] == 1 then
            return false  -- Cycle detected
        end
        if visited[vertex] == 2 then
            return true   -- Already processed
        end
        
        visited[vertex] = 1  -- Mark as visiting
        
        -- Visit all neighbors
        for _, neighbor in ipairs(adj_list[vertex]) do
            if not dfs(neighbor) then
                return false
            end
        end
        
        visited[vertex] = 2  -- Mark as visited
        return true
    end
    
    -- Check all vertices
    for i = 1, num_vertices do
        if visited[i] == 0 then
            if not dfs(i) then
                return false  -- Cycle found
            end
        end
    end
    
    return true  -- No cycles found
end

-- Main function to solve the problem
function solve_acyclicity(num_vertices, edges)
    -- Using the BFS approach (Kahn's algorithm)
    return is_acyclic(num_vertices, edges)
end

-- Example usage:
-- For testing purposes
local test_edges = {
    {1, 2},
    {2, 3},
    {3, 1}
}

-- Test with a simple case
print("Testing acyclicity:")
print("Graph with cycle:", solve_acyclicity(3, test_edges))  -- Should return false

-- Test with acyclic graph
local acyclic_edges = {
    {1, 2},
    {2, 3},
    {1, 3}
}

print("Acyclic graph:", solve_acyclicity(3, acyclic_edges))  -- Should return true

-- For Rosalind input format, you would typically read from file:
-- function read_rosalind_input(filename)
--     -- Read file and parse input
--     -- Return num_vertices and edges
-- end
```

## Key Points

1. **Time Complexity**: O(V + E) where V is vertices and E is edges
2. **Space Complexity**: O(V + E) for storing adjacency list and in-degrees
3. **Algorithm Choice**: I've provided two approaches:
   - Kahn's algorithm (BFS-based) - more intuitive
   - DFS-based approach - more commonly used in interviews
4. **Return Value**: 
   - `true` if the graph is acyclic (DAG)
   - `false` if the graph contains cycles

## Usage for Rosalind

To use this for Rosalind's specific input format, you would need to:
1. Parse the input file to extract number of vertices and edges
2. Read the edge list
3. Call `solve_acyclicity(num_vertices, edges)`
4. Return 1 if acyclic, 0 if cyclic

The solution correctly identifies whether a directed graph contains cycles by implementing topological sorting algorithms.

