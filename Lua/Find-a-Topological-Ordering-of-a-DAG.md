# Rosalind Problem: Find a Topological Ordering of a DAG

## Problem Understanding

Given a directed acyclic graph (DAG), we need to find a topological ordering of its vertices. A topological ordering is a linear ordering of vertices such that for every directed edge u → v, vertex u comes before vertex v in the ordering.

## Approach

I'll use Kahn's algorithm:
1. Calculate in-degrees for all vertices
2. Add all vertices with in-degree 0 to a queue
3. While queue is not empty:
   - Remove vertex from queue
   - Add it to result
   - For each neighbor, decrease in-degree
   - If in-degree becomes 0, add to queue

## Solution

```lua
function find_topological_ordering(edges, num_vertices)
    -- Build adjacency list and calculate in-degrees
    local adj_list = {}
    local in_degree = {}
    
    -- Initialize in-degrees
    for i = 1, num_vertices do
        in_degree[i] = 0
        adj_list[i] = {}
    end
    
    -- Build adjacency list and in-degrees
    for _, edge in ipairs(edges) do
        local from, to = edge[1], edge[2]
        table.insert(adj_list[from], to)
        in_degree[to] = in_degree[to] + 1
    end
    
    -- Find vertices with in-degree 0
    local queue = {}
    local result = {}
    
    for i = 1, num_vertices do
        if in_degree[i] == 0 then
            table.insert(queue, i)
        end
    end
    
    -- Process vertices
    while #queue > 0 do
        local current = table.remove(queue, 1)
        table.insert(result, current)
        
        -- Process neighbors
        for _, neighbor in ipairs(adj_list[current]) do
            in_degree[neighbor] = in_degree[neighbor] - 1
            if in_degree[neighbor] == 0 then
                table.insert(queue, neighbor)
            end
        end
    end
    
    return result
end

-- Read input from stdin
local input_lines = {}
for line in io.lines() do
    table.insert(input_lines, line)
end

-- Parse input
local num_vertices, num_edges = string.match(input_lines[1], "(%d+)%s+(%d+)")
num_vertices = tonumber(num_vertices)
num_edges = tonumber(num_edges)

local edges = {}
for i = 2, #input_lines do
    local line = input_lines[i]
    if line ~= "" then
        local from, to = string.match(line, "(%d+)%s+(%d+)")
        table.insert(edges, {tonumber(from), tonumber(to)})
    end
end

-- Find topological ordering
local ordering = find_topological_ordering(edges, num_vertices)

-- Output result
for i, vertex in ipairs(ordering) do
    io.write(vertex)
    if i < #ordering then
        io.write(" ")
    end
end
io.write("\n")
```

## Example Usage

For input:
```
6 8
5 2
5 0
4 0
4 1
2 3
3 1
1 3
3 4
```

The algorithm would output one valid topological ordering, such as:
```
5 4 2 3 1 0
```

## Time and Space Complexity

- **Time Complexity**: O(V + E) where V is the number of vertices and E is the number of edges
- **Space Complexity**: O(V + E) for storing the adjacency list and in-degrees

## Key Points

1. The algorithm uses Kahn's method with in-degrees
2. Vertices with in-degree 0 are processed first
3. When a vertex is processed, its neighbors have their in-degrees reduced
4. If a neighbor's in-degree becomes 0, it's added to the processing queue
5. The result is a valid topological ordering if the graph is acyclic

