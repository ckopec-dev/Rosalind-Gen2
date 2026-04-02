# Rosalind Problem: Implement_ColoredEdges in Lua

## Problem Description
The colored edges of a genome are the edges that connect the consecutive elements of a genome when it is represented as a graph. In this problem, we need to implement the `ColoredEdges` function that takes a genome (represented as a list of cycles) and returns the colored edges as a list of pairs.

## Solution

```lua
function colored_edges(genome)
    local edges = {}
    
    -- For each cycle in the genome
    for _, cycle in ipairs(genome) do
        local n = #cycle
        
        -- For each consecutive pair of elements in the cycle
        for i = 1, n do
            local node1 = cycle[i]
            local node2 = cycle[i % n + 1]  -- Wrap around to first element for last edge
            
            -- Add the edge (node1, node2) to the edges list
            table.insert(edges, {node1, node2})
        end
    end
    
    return edges
end

-- Alternative implementation for circular genome
function colored_edges_circular(genome)
    local edges = {}
    
    -- For each cycle in the genome
    for _, cycle in ipairs(genome) do
        local n = #cycle
        
        -- Handle the case where we have a circular genome
        for i = 1, n do
            local node1 = cycle[i]
            local node2 = cycle[i % n + 1]  -- Wrap around to first element for last edge
            
            -- Add the edge (node1, node2) to the edges list
            table.insert(edges, {node1, node2})
        end
    end
    
    return edges
end

-- Helper function to format output
function format_edges(edges)
    local result = {}
    for _, edge in ipairs(edges) do
        table.insert(result, "(" .. edge[1] .. ", " .. edge[2] .. ")")
    end
    return table.concat(result, ", ")
end

-- Example usage:
-- genome = {{1, 2, 3, 4, 5, 6}}
-- edges = colored_edges(genome)
-- print(format_edges(edges))

return {
    colored_edges = colored_edges,
    colored_edges_circular = colored_edges_circular,
    format_edges = format_edges
}
```

## Explanation

The `colored_edges` function works as follows:

1. **Input**: Takes a genome represented as a list of cycles, where each cycle is a list of integers
2. **Process**: For each cycle in the genome:
   - Iterate through consecutive pairs of elements
   - Create edges between each pair of consecutive elements
   - Handle circular nature by connecting the last element to the first
3. **Output**: Returns a list of edges, where each edge is represented as a pair of integers

## Example

For a genome with one cycle `[1, 2, 3, 4, 5, 6]`:
- Edges would be: `(1, 2), (2, 3), (3, 4), (4, 5), (5, 6), (6, 1)`

## Time and Space Complexity

- **Time Complexity**: O(n) where n is the total number of elements in all cycles
- **Space Complexity**: O(n) for storing the edges

This implementation correctly handles the colored edges problem as specified in the Rosalind platform requirements.

