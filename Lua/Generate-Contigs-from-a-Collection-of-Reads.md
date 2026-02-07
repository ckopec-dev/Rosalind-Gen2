# Rosalind Problem: Generate Contigs from a Collection of Reads (Lua Solution)

## Problem Understanding

Given a collection of DNA reads, we need to construct contigs (maximal non-branching paths) from the de Bruijn graph of the reads.

## Solution Approach

1. Build a de Bruijn graph from the reads
2. Find all maximal non-branching paths
3. Convert paths to contigs

## Implementation

```lua
function generate_contigs(reads)
    -- Build de Bruijn graph
    local graph = {}
    local in_degree = {}
    local out_degree = {}
    
    -- For each read, create edges
    for _, read in ipairs(reads) do
        if #read >= 2 then
            local prefix = read:sub(1, #read - 1)
            local suffix = read:sub(2, #read)
            
            -- Add edge from prefix to suffix
            if not graph[prefix] then
                graph[prefix] = {}
            end
            table.insert(graph[prefix], suffix)
            
            -- Update degrees
            if not out_degree[prefix] then
                out_degree[prefix] = 0
            end
            out_degree[prefix] = out_degree[prefix] + 1
            
            if not in_degree[suffix] then
                in_degree[suffix] = 0
            end
            in_degree[suffix] = in_degree[suffix] + 1
        end
    end
    
    -- Find nodes with unequal in and out degrees
    local unbalanced_nodes = {}
    for node in pairs(out_degree) do
        if (in_degree[node] or 0) ~= (out_degree[node] or 0) then
            table.insert(unbalanced_nodes, node)
        end
    end
    
    -- Find all maximal non-branching paths
    local contigs = {}
    local visited = {}
    
    -- Helper function to find all paths from a node
    local function find_paths(start_node)
        local path = {start_node}
        local current_node = start_node
        
        while true do
            -- Check if we can continue the path
            if not graph[current_node] or #graph[current_node] == 0 then
                break
            end
            
            -- If node has exactly one outgoing edge, continue
            if #graph[current_node] == 1 then
                local next_node = graph[current_node][1]
                table.insert(path, next_node)
                current_node = next_node
            else
                -- Multiple outgoing edges - stop here
                break
            end
        end
        
        return path
    end
    
    -- Find all maximal non-branching paths
    local all_paths = {}
    
    -- Start with nodes that have in-degree ≠ out-degree
    for _, node in ipairs(unbalanced_nodes) do
        if out_degree[node] and out_degree[node] > 0 then
            local path = find_paths(node)
            table.insert(all_paths, path)
        end
    end
    
    -- Find isolated cycles
    local visited_nodes = {}
    for node in pairs(out_degree) do
        if not visited_nodes[node] and out_degree[node] and out_degree[node] > 0 then
            local current = node
            local cycle = {current}
            visited_nodes[current] = true
            
            -- Follow the cycle
            while graph[current] and #graph[current] == 1 do
                local next_node = graph[current][1]
                if next_node == node then
                    -- Found a cycle
                    table.insert(all_paths, cycle)
                    break
                else
                    table.insert(cycle, next_node)
                    visited_nodes[next_node] = true
                    current = next_node
                end
            end
        end
    end
    
    -- Convert paths to contigs
    local contigs = {}
    for _, path in ipairs(all_paths) do
        if #path > 0 then
            local contig = path[1]
            for i = 2, #path do
                contig = contig .. path[i]:sub(#path[i], #path[i])
            end
            table.insert(contigs, contig)
        end
    end
    
    return contigs
end

-- Alternative cleaner implementation
function generate_contigs_clean(reads)
    -- Build de Bruijn graph
    local edges = {}
    local in_degree = {}
    local out_degree = {}
    
    -- Create edges from reads
    for _, read in ipairs(reads) do
        if #read >= 2 then
            local prefix = read:sub(1, #read - 1)
            local suffix = read:sub(2, #read)
            
            -- Add edge
            if not edges[prefix] then
                edges[prefix] = {}
            end
            table.insert(edges[prefix], suffix)
            
            -- Update degrees
            out_degree[prefix] = (out_degree[prefix] or 0) + 1
            in_degree[suffix] = (in_degree[suffix] or 0) + 1
        end
    end
    
    -- Find all maximal non-branching paths
    local contigs = {}
    local visited = {}
    
    -- Helper to build contig from path
    local function build_contig(path)
        if #path == 0 then return "" end
        local contig = path[1]
        for i = 2, #path do
            contig = contig .. path[i]:sub(-1)
        end
        return contig
    end
    
    -- Find all paths starting from unbalanced nodes
    local unbalanced = {}
    for node in pairs(out_degree) do
        local in_deg = in_degree[node] or 0
        local out_deg = out_degree[node] or 0
        if in_deg ~= out_deg then
            table.insert(unbalanced, node)
        end
    end
    
    -- Find all maximal non-branching paths
    for _, start_node in ipairs(unbalanced) do
        if out_degree[start_node] and out_degree[start_node] > 0 then
            local path = {start_node}
            local current = start_node
            
            -- Extend path while possible
            while edges[current] and #edges[current] == 1 do
                local next_node = edges[current][1]
                table.insert(path, next_node)
                current = next_node
            end
            
            -- Add to contigs
            local contig = build_contig(path)
            if contig ~= "" then
                table.insert(contigs, contig)
            end
        end
    end
    
    -- Handle isolated cycles
    local all_nodes = {}
    for node in pairs(out_degree) do
        table.insert(all_nodes, node)
    end
    
    local visited = {}
    for _, node in ipairs(all_nodes) do
        if not visited[node] and out_degree[node] and out_degree[node] > 0 then
            local cycle = {}
            local current = node
            local start = node
            
            repeat
                table.insert(cycle, current)
                visited[current] = true
                if edges[current] and #edges[current] == 1 then
                    current = edges[current][1]
                else
                    break
                end
            until current == start
            
            if #cycle > 0 and cycle[1] == cycle[#cycle] then
                local contig = build_contig(cycle)
                if contig ~= "" then
                    table.insert(contigs, contig)
                end
            end
        end
    end
    
    return contigs
end

-- Simple working implementation for the problem
function generate_contigs_simple(reads)
    -- Build de Bruijn graph
    local graph = {}
    local in_degree = {}
    local out_degree = {}
    
    for _, read in ipairs(reads) do
        if #read >= 2 then
            local prefix = read:sub(1, #read - 1)
            local suffix = read:sub(2, #read)
            
            if not graph[prefix] then
                graph[prefix] = {}
            end
            table.insert(graph[prefix], suffix)
            
            out_degree[prefix] = (out_degree[prefix] or 0) + 1
            in_degree[suffix] = (in_degree[suffix] or 0) + 1
        end
    end
    
    -- Find all maximal non-branching paths
    local contigs = {}
    local visited = {}
    
    -- Helper function to get contig from path
    local function path_to_contig(path)
        if #path == 0 then return "" end
        local contig = path[1]
        for i = 2, #path do
            contig = contig .. path[i]:sub(-1)
        end
        return contig
    end
    
    -- Find all nodes with unequal degrees
    local start_nodes = {}
    for node in pairs(out_degree) do
        local in_deg = in_degree[node] or 0
        local out_deg = out_degree[node] or 0
        if in_deg ~= out_deg then
            table.insert(start_nodes, node)
        end
    end
    
    -- Generate contigs
    for _, start in ipairs(start_nodes) do
        if out_degree[start] and out_degree[start] > 0 then
            local path = {start}
            local current = start
            
            -- Extend path while it's a linear path
            while graph[current] and #graph[current] == 1 do
                local next_node = graph[current][1]
                table.insert(path, next_node)
                current = next_node
            end
            
            local contig = path_to_contig(path)
            if contig ~= "" then
                table.insert(contigs, contig)
            end
        end
    end
    
    return contigs
end

-- Example usage:
local reads = {
    "ATGCGAT",
    "TGCATGC",
    "GCATGCA",
    "CATGCAT",
    "ATGCATG",
    "TGCATGC"
}

local contigs = generate_contigs_simple(reads)
for _, contig in ipairs(contigs) do
    print(contig)
end
```

## Explanation

This solution implements the contig generation algorithm for de Bruijn graphs:

1. **Graph Construction**: For each read, we create a de Bruijn graph where each node is a k-1 mer and edges represent overlaps between consecutive k-mers.

2. **Degree Calculation**: We calculate in-degrees and out-degrees for each node to identify unbalanced nodes.

3. **Maximal Non-Branching Paths**: 
   - Start from nodes with unequal in/out degrees
   - Follow paths while each node has exactly one outgoing edge
   - Stop when encountering branching points or cycles

4. **Contig Generation**: Convert each path into a contig by concatenating the first k-1 mer and the last character of each subsequent k-mer.

## Time Complexity
- O(n × k) where n is the number of reads and k is the read length

## Space Complexity  
- O(n × k) for storing the graph and results

The solution handles the core requirements of generating contigs from DNA reads using de Bruijn graph construction and maximal non-branching path finding.

