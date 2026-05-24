# Rosalind Problem: Finding a Protein Motif (Finding a Protein Motif)

## Problem Description
Given: At most 200 DNA strings of length at most 200 bp in FASTA format.

Return: The position and length of every motif in the DNA strings.

## Solution in Lua

```lua
-- Function to find protein motifs in DNA sequences
function find_protein_motifs(fasta_data)
    -- Define the protein motif pattern (N{P}[ST][^P])
    -- This is a regular expression pattern for the motif
    local motif_pattern = "N[^P][ST][^P]"
    
    -- Process each sequence in the FASTA data
    local results = {}
    local current_id = ""
    local current_seq = ""
    
    -- Split FASTA data into lines
    local lines = {}
    for line in fasta_data:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end
    
    -- Parse FASTA format
    for i, line in ipairs(lines) do
        if line:sub(1, 1) == ">" then
            -- Save previous sequence if exists
            if current_id ~= "" and current_seq ~= "" then
                -- Find all motif positions in current sequence
                local positions = {}
                local start = 1
                
                while true do
                    local pos = current_seq:find(motif_pattern, start)
                    if not pos then break end
                    
                    -- Add position (1-indexed) and length
                    table.insert(positions, pos)
                    start = pos + 1
                end
                
                if #positions > 0 then
                    table.insert(results, {id = current_id, positions = positions})
                end
                
                current_seq = ""
            end
            
            -- Set new sequence ID
            current_id = line:sub(2)
        else
            -- Append sequence data
            current_seq = current_seq .. line
        end
    end
    
    -- Handle last sequence
    if current_id ~= "" and current_seq ~= "" then
        local positions = {}
        local start = 1
        
        while true do
            local pos = current_seq:find(motif_pattern, start)
            if not pos then break end
            
            table.insert(positions, pos)
            start = pos + 1
        end
        
        if #positions > 0 then
            table.insert(results, {id = current_id, positions = positions})
        end
    end
    
    return results
end

-- Alternative implementation using string.gsub for position tracking
function find_motifs_alternative(fasta_data)
    local results = {}
    local current_id = ""
    local current_seq = ""
    
    -- Split FASTA data into lines
    local lines = {}
    for line in fasta_data:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end
    
    -- Parse FASTA format
    for i, line in ipairs(lines) do
        if line:sub(1, 1) == ">" then
            if current_id ~= "" and current_seq ~= "" then
                -- Find all motif positions
                local positions = {}
                local pos = 1
                
                while pos <= #current_seq - 3 do  -- -3 because motif is 4 characters
                    local substring = current_seq:sub(pos, pos + 3)
                    if #substring == 4 then
                        if matches_motif(substring) then
                            table.insert(positions, pos)
                        end
                    end
                    pos = pos + 1
                end
                
                if #positions > 0 then
                    table.insert(results, {id = current_id, positions = positions})
                end
                current_seq = ""
            end
            current_id = line:sub(2)
        else
            current_seq = current_seq .. line
        end
    end
    
    -- Handle last sequence
    if current_id ~= "" and current_seq ~= "" then
        local positions = {}
        local pos = 1
        
        while pos <= #current_seq - 3 do
            local substring = current_seq:sub(pos, pos + 3)
            if #substring == 4 then
                if matches_motif(substring) then
                    table.insert(positions, pos)
                end
            end
            pos = pos + 1
        end
        
        if #positions > 0 then
            table.insert(results, {id = current_id, positions = positions})
        end
    end
    
    return results
end

-- Helper function to check if substring matches motif
function matches_motif(substring)
    -- N{P}[ST][^P] - N not P, S or T, not P
    if #substring ~= 4 then return false end
    
    local first = substring:sub(1, 1)
    local second = substring:sub(2, 2)
    local third = substring:sub(3, 3)
    local fourth = substring:sub(4, 4)
    
    -- First character must be N
    if first ~= "N" then return false end
    
    -- Second character must NOT be P
    if second == "P" then return false end
    
    -- Third character must be S or T
    if third ~= "S" and third ~= "T" then return false end
    
    -- Fourth character must NOT be P
    if fourth == "P" then return false end
    
    return true
end

-- Main function to solve the problem
function solve_finding_a_protein_motif(input_data)
    -- Process input and find motifs
    local results = find_motifs_alternative(input_data)
    
    -- Print results
    for _, result in ipairs(results) do
        print(result.id)
        if #result.positions > 0 then
            local positions = {}
            for _, pos in ipairs(result.positions) do
                table.insert(positions, tostring(pos))
            end
            print(table.concat(positions, " "))
        else
            print() -- Empty line if no motifs
        end
    end
    
    return results
end

-- Example usage:
-- local input = [[
-- >Rosalind_1234
-- MNGTEGPNLGGVTVGAVITGAVVGVVGVV
-- >Rosalind_5678
-- NASTP
-- ]]

-- solve_finding_a_protein_motif(input)
```

## Explanation

This solution implements a parser for FASTA formatted data and finds protein motifs in DNA sequences. The key aspects are:

1. **FASTA Parsing**: The function reads FASTA format data, separating sequence IDs from sequence data
2. **Motif Pattern**: The protein motif is defined as N{P}[ST][^P] where:
   - N is any amino acid
   - {P} means NOT P (any amino acid except proline)
   - [ST] means either Serine or Threonine
   - [^P] means NOT P (any amino acid except proline)
3. **Position Finding**: For each sequence, it scans through all possible 4-character substrings to find matches
4. **Output Format**: Results are printed with sequence ID followed by space-separated positions (1-indexed)

## Usage

```lua
-- Example input
local input = [[
>Rosalind_001
MNGTEGPNLGGVTVGAVITGAVVGVVGVV
>Rosalind_002
NASTP
]]

solve_finding_a_protein_motif(input)
```

The solution handles multiple sequences in FASTA format and correctly identifies motif positions according to the specified pattern.

