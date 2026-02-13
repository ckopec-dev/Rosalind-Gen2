# Rosalind Problem: Genome Assembly as Shortest Superstring

## Problem Understanding

The goal is to find the shortest superstring that contains all given DNA sequences as substrings. This is a variant of the Shortest Superstring Problem, which is NP-hard. For small inputs, we can use a greedy approach or brute force.

## Approach

I'll implement a greedy algorithm that:
1. Finds the maximum overlap between any two strings
2. Merges the two strings with maximum overlap
3. Repeats until only one string remains

## Solution

```lua
function overlap(a, b)
    -- Find the maximum overlap between the end of string a and the beginning of string b
    local max_overlap = 0
    local min_len = math.min(#a, #b)
    
    -- Check all possible overlaps from longest to shortest
    for i = min_len, 1, -1 do
        if string.sub(a, #a - i + 1, #a) == string.sub(b, 1, i) then
            max_overlap = i
            break
        end
    end
    
    return max_overlap
end

function merge_strings(a, b, overlap_len)
    -- Merge string a and b with given overlap
    if overlap_len == 0 then
        return a .. b
    else
        return a .. string.sub(b, overlap_len + 1, #b)
    end
end

function find_shortest_superstring(strings)
    if #strings == 0 then return "" end
    if #strings == 1 then return strings[1] end
    
    -- Create a matrix of overlaps between all pairs
    local n = #strings
    local overlap_matrix = {}
    for i = 1, n do
        overlap_matrix[i] = {}
        for j = 1, n do
            if i == j then
                overlap_matrix[i][j] = 0
            else
                overlap_matrix[i][j] = overlap(strings[i], strings[j])
            end
        end
    end
    
    -- Greedy approach: keep merging strings with maximum overlap
    while #strings > 1 do
        local max_overlap = -1
        local i_merge, j_merge = 1, 2
        
        -- Find the pair with maximum overlap
        for i = 1, #strings do
            for j = 1, #strings do
                if i ~= j and overlap_matrix[i][j] > max_overlap then
                    max_overlap = overlap_matrix[i][j]
                    i_merge, j_merge = i, j
                end
            end
        end
        
        -- If no positive overlap found, just concatenate
        if max_overlap <= 0 then
            strings[1] = strings[1] .. strings[2]
            table.remove(strings, 2)
        else
            -- Merge strings at positions i_merge and j_merge
            local merged = merge_strings(strings[i_merge], strings[j_merge], max_overlap)
            
            -- Remove the two strings and add the merged one
            table.remove(strings, j_merge)
            table.remove(strings, i_merge)
            table.insert(strings, merged)
            
            -- Update overlap matrix
            for k = 1, #strings do
                if k ~= #strings then
                    overlap_matrix[k][#strings] = overlap(strings[k], strings[#strings])
                    overlap_matrix[#strings][k] = overlap(strings[#strings], strings[k])
                end
            end
        end
    end
    
    return strings[1]
end

function solve_rosalind_problem(input_data)
    -- Parse input data (assuming it's a list of strings)
    local strings = {}
    for line in string.gmatch(input_data, "[^\r\n]+") do
        if line ~= "" and not string.match(line, "^>") then
            table.insert(strings, line)
        end
    end
    
    return find_shortest_superstring(strings)
end

-- Example usage:
-- Input format would be FASTA-like format with > labels
-- For example:
local sample_input = [[>Rosalind_56
ATTAGACCTG
>Rosalind_57
CTTGACCCTA
>Rosalind_58
AGACCTGCCG
>Rosalind_59
GCCGGAATAC]]

-- Since the input format is not specified exactly, here's a more robust version:
function parse_fasta(input)
    local sequences = {}
    local current_id = ""
    local current_seq = ""
    
    for line in string.gmatch(input, "[^\r\n]+") do
        if string.sub(line, 1, 1) == ">" then
            if current_id ~= "" then
                table.insert(sequences, current_seq)
            end
            current_id = string.sub(line, 2)
            current_seq = ""
        else
            current_seq = current_seq .. line
        end
    end
    
    if current_id ~= "" then
        table.insert(sequences, current_seq)
    end
    
    return sequences
end

-- Complete solution function
function genome_assembly_shortest_superstring(input_data)
    local sequences = parse_fasta(input_data)
    return find_shortest_superstring(sequences)
end

-- Test with example data
local test_input = [[>Rosalind_56
ATTAGACCTG
>Rosalind_57
CTTGACCCTA
>Rosalind_58
AGACCTGCCG
>Rosalind_59
GCCGGAATAC]]

-- For a real Rosalind submission, you would read from file or standard input
-- print(genome_assembly_shortest_superstring(test_input))
```

## Key Points

1. **Overlap Function**: Calculates the maximum overlap between the end of one string and the beginning of another
2. **Merge Function**: Combines two strings by overlapping them optimally
3. **Greedy Strategy**: Repeatedly merge the pair of strings with maximum overlap
4. **Matrix Approach**: Maintains overlap information between all pairs for efficient lookup

## Time Complexity
- O(n³) where n is the number of strings
- The greedy approach is not guaranteed to find the optimal solution but works well in practice for many cases

## Space Complexity
- O(n²) for the overlap matrix

This solution handles the core problem of finding the shortest superstring that contains all given DNA sequences as substrings, which is fundamental to genome assembly problems.

