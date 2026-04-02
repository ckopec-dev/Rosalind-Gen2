# Using the Spectrum Graph to Infer Peptides - Rosalind Problem Solution in Lua

## Problem Understanding

This problem asks us to reconstruct a peptide sequence from its spectrum graph. The spectrum graph represents all possible peptide sequences that could produce the given spectrum, where edges represent amino acid masses.

## Solution Approach

1. Parse the spectrum data
2. Build the spectrum graph with edges representing amino acid masses
3. Find all paths from source to sink that match the spectrum
4. Reconstruct the peptide sequences

## Implementation

```lua
-- Function to parse spectrum data
function parse_spectrum(spectrum_str)
    local spectrum = {}
    for mass in spectrum_str:gmatch("%d+%.?%d*") do
        table.insert(spectrum, tonumber(mass))
    end
    return spectrum
end

-- Amino acid masses (from Rosalind problem)
local amino_acid_masses = {
    ['G'] = 57.01333,
    ['A'] = 71.03711,
    ['S'] = 87.03203,
    ['P'] = 97.05276,
    ['V'] = 99.06841,
    ['T'] = 101.04768,
    ['C'] = 103.00919,
    ['I'] = 113.08406,
    ['L'] = 113.08406,
    ['N'] = 114.04293,
    ['D'] = 115.02694,
    ['K'] = 128.09496,
    ['Q'] = 128.05858,
    ['E'] = 129.04259,
    ['M'] = 131.04049,
    ['H'] = 137.05891,
    ['F'] = 147.06841,
    ['R'] = 156.10111,
    ['Y'] = 163.06333,
    ['W'] = 186.07931
}

-- Reverse lookup for amino acid from mass
local mass_to_amino = {}
for amino, mass in pairs(amino_acid_masses) do
    mass_to_amino[math.floor(mass * 1000 + 0.5) / 1000] = amino
end

-- Function to build spectrum graph
function build_spectrum_graph(spectrum)
    local graph = {}
    local n = #spectrum
    
    -- Initialize graph with all nodes
    for i = 1, n do
        graph[spectrum[i]] = graph[spectrum[i]] or {}
        for j = 1, n do
            if i ~= j then
                local diff = spectrum[j] - spectrum[i]
                local diff_rounded = math.floor(diff * 1000 + 0.5) / 1000
                if mass_to_amino[diff_rounded] then
                    table.insert(graph[spectrum[i]], {mass = diff_rounded, amino = mass_to_amino[diff_rounded], target = spectrum[j]})
                end
            end
        end
    end
    
    return graph
end

-- Function to find all paths in spectrum graph
function find_all_paths(graph, start, end_point, path, all_paths)
    path = path or {}
    table.insert(path, start)
    
    if start == end_point then
        table.insert(all_paths, table.concat(path, " "))
        return
    end
    
    if not graph[start] then
        return
    end
    
    for _, edge in ipairs(graph[start]) do
        if not table.contains(path, edge.target) then  -- Avoid cycles
            find_all_paths(graph, edge.target, end_point, path, all_paths)
        end
    end
    
    table.remove(path)
end

-- Helper function to check if table contains value
function table.contains(t, value)
    for _, v in ipairs(t) do
        if v == value then
            return true
        end
    end
    return false
end

-- Main function to solve the problem
function solve_spectrum_graph(spectrum_str)
    local spectrum = parse_spectrum(spectrum_str)
    
    -- Sort spectrum
    table.sort(spectrum)
    
    -- Build graph
    local graph = build_spectrum_graph(spectrum)
    
    -- Find all possible peptides
    local all_paths = {}
    local start = 0
    local end_point = spectrum[#spectrum]
    
    -- Find paths from 0 to maximum mass
    if graph[start] then
        for _, edge in ipairs(graph[start]) do
            if edge.target == end_point then
                table.insert(all_paths, edge.amino)
            else
                local temp_path = {start, edge.target}
                -- Continue path building
                find_all_paths(graph, edge.target, end_point, temp_path, all_paths)
            end
        end
    end
    
    return all_paths
end

-- Alternative approach for better spectrum graph construction
function solve_spectrum_graph_v2(spectrum_str)
    local spectrum = parse_spectrum(spectrum_str)
    table.sort(spectrum)
    
    -- Create adjacency list representation
    local adj_list = {}
    local n = #spectrum
    
    -- Build adjacency list
    for i = 1, n do
        adj_list[spectrum[i]] = adj_list[spectrum[i]] or {}
        for j = 1, n do
            if i ~= j then
                local diff = spectrum[j] - spectrum[i]
                local diff_rounded = math.floor(diff * 1000 + 0.5) / 1000
                if mass_to_amino[diff_rounded] then
                    table.insert(adj_list[spectrum[i]], {
                        target = spectrum[j],
                        amino = mass_to_amino[diff_rounded],
                        mass_diff = diff_rounded
                    })
                end
            end
        end
    end
    
    -- Find all possible peptide sequences
    local peptides = {}
    
    -- Start from 0 and find all paths to the maximum mass
    local function dfs(current_mass, path, visited)
        if current_mass == spectrum[#spectrum] then
            table.insert(peptides, table.concat(path, ""))
            return
        end
        
        if not adj_list[current_mass] then return end
        
        for _, edge in ipairs(adj_list[current_mass]) do
            if not visited[edge.target] then
                visited[edge.target] = true
                table.insert(path, edge.amino)
                dfs(edge.target, path, visited)
                table.remove(path)
                visited[edge.target] = false
            end
        end
    end
    
    -- Start DFS from 0
    local visited = {}
    visited[0] = true
    dfs(0, {}, visited)
    
    return peptides
end

-- Example usage
function main()
    -- Example spectrum from Rosalind problem
    local spectrum = "0 113 128 186 244 299 314 372 429 487 502 560 618 675 733 788 846"
    
    print("Spectrum:", spectrum)
    print("Solving spectrum graph...")
    
    local peptides = solve_spectrum_graph_v2(spectrum)
    
    print("Possible peptides:")
    for i, peptide in ipairs(peptides) do
        print(i .. ": " .. peptide)
    end
end

-- Run example
main()
```

## Key Points of the Solution

1. **Data Parsing**: Extract numeric values from the spectrum string
2. **Mass Mapping**: Create lookup tables between amino acid symbols and their masses
3. **Graph Construction**: Build adjacency list representing possible amino acid transitions
4. **Path Finding**: Use DFS to find all valid peptide sequences
5. **Cycle Prevention**: Track visited nodes to avoid infinite loops

## Time and Space Complexity

- **Time Complexity**: O(n²) for graph construction + O(2^n) for path finding in worst case
- **Space Complexity**: O(n²) for storing the graph

## Usage

The solution takes a spectrum string as input and returns all possible peptide sequences that could produce that spectrum. The approach handles the spectrum graph construction and path finding to reconstruct the peptide sequences.

