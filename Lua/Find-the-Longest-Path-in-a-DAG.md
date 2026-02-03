# Rosalind Problem: Find the Longest Path in a DAG

## Problem Understanding

We need to find the longest path in a directed acyclic graph (DAG) from a source node to a sink node. This is a classic dynamic programming problem on DAGs.

## Approach

1. **Topological Sort**: First, we need to sort the nodes in topological order
2. **Dynamic Programming**: Use DP to compute the longest path to each node
3. **Path Reconstruction**: Track the actual path for output

## Solution

```lua
-- Function to perform topological sort using Kahn's algorithm
function topological_sort(graph, nodes)
    local in_degree = {}
    for node in pairs(nodes) do
        in_degree[node] = 0
    end
    
    -- Calculate in-degrees
    for node in pairs(nodes) do
        for _, neighbor in ipairs(graph[node] or {}) do
            in_degree[neighbor] = in_degree[neighbor] + 1
        end
    end
    
    -- Initialize queue with nodes of in-degree 0
    local queue = {}
    for node, degree in pairs(in_degree) do
        if degree == 0 then
            table.insert(queue, node)
        end
    end
    
    local topo_order = {}
    while #queue > 0 do
        local current = table.remove(queue, 1)
        table.insert(topo_order, current)
        
        -- Process neighbors
        for _, neighbor in ipairs(graph[current] or {}) do
            in_degree[neighbor] = in_degree[neighbor] - 1
            if in_degree[neighbor] == 0 then
                table.insert(queue, neighbor)
            end
        end
    end
    
    return topo_order
end

-- Function to find longest path in DAG
function longest_path_dag(graph, weights, source, sink)
    -- Get topological order
    local all_nodes = {}
    for node in pairs(graph) do
        all_nodes[node] = true
    end
    for node in pairs(weights) do
        all_nodes[node] = true
    end
    
    local topo_order = topological_sort(graph, all_nodes)
    
    -- Initialize DP arrays
    local dist = {}
    local prev = {}
    
    -- Initialize all distances to negative infinity
    for node in pairs(all_nodes) do
        dist[node] = -math.huge
        prev[node] = nil
    end
    
    -- Set source distance to 0
    dist[source] = 0
    
    -- Process nodes in topological order
    for _, node in ipairs(topo_order) do
        if dist[node] ~= -math.huge then
            -- Process all neighbors
            for _, neighbor in ipairs(graph[node] or {}) do
                local edge_weight = weights[node .. "," .. neighbor] or 0
                local new_dist = dist[node] + edge_weight
                
                if new_dist > dist[neighbor] then
                    dist[neighbor] = new_dist
                    prev[neighbor] = node
                end
            end
        end
    end
    
    -- Reconstruct path
    local path = {}
    local current = sink
    while current ~= nil do
        table.insert(path, 1, current)
        current = prev[current]
    end
    
    return dist[sink], path
end

-- Main function to solve the problem
function solve_longest_path(input_lines)
    -- Parse input
    local source = tonumber(input_lines[1])
    local sink = tonumber(input_lines[2])
    
    local graph = {}
    local weights = {}
    
    local i = 3
    while i <= #input_lines do
        local line = input_lines[i]
        if line == "" then
            i = i + 1
            break
        end
        
        local parts = {}
        for part in string.gmatch(line, "%S+") do
            table.insert(parts, part)
        end
        
        local from_node = tonumber(parts[1])
        local to_node = tonumber(parts[2])
        local weight = tonumber(parts[3])
        
        if not graph[from_node] then
            graph[from_node] = {}
        end
        table.insert(graph[from_node], to_node)
        
        weights[from_node .. "," .. to_node] = weight
        
        i = i + 1
    end
    
    -- Find longest path
    local max_length, path = longest_path_dag(graph, weights, source, sink)
    
    -- Return result
    return tostring(max_length), table.concat(path, " ")
end

-- Example usage (for testing)
function main()
    -- Example input
    local example_input = {
        "0",
        "4",
        "0 1 7",
        "0 2 4",
        "1 2 3",
        "1 3 2",
        "2 3 5",
        "2 4 6",
        "3 4 1"
    }
    
    local max_length, path = solve_longest_path(example_input)
    print("Maximum length: " .. max_length)
    print("Path: " .. path)
end

-- Uncomment the line below to run example
-- main()
```

## Key Points

1. **Topological Sorting**: We use Kahn's algorithm to get nodes in topological order
2. **Dynamic Programming**: For each node in topological order, we update distances to neighbors
3. **Weight Storage**: We store edge weights in a hash map with key format "from,to"
4. **Path Reconstruction**: We maintain a `prev` array to reconstruct the actual path

## Time Complexity
- Topological sort: O(V + E)
- DP computation: O(V + E)
- Overall: O(V + E) where V is vertices and E is edges

## Space Complexity
- O(V + E) for storing graph, weights, and DP arrays

This solution efficiently finds the longest path in a DAG by leveraging the topological ordering property to ensure we process nodes in the correct order for dynamic programming.

