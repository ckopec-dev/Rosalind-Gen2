# Rosalind Problem: Isolating Symbols in Alignments (Lua Solution)

## Problem Understanding

The problem asks us to find the symbols that appear in at least one of the sequences in a multiple sequence alignment, and count how many times each symbol appears across all sequences.

## Solution Approach

1. Parse the input to extract sequences
2. Iterate through each sequence and count symbol occurrences
3. Output the symbols that appear in at least one sequence

## Lua Implementation

```lua
-- Function to solve Isolating Symbols in Alignments
function isolate_symbols(fasta_data)
    local symbol_count = {}
    
    -- Parse FASTA format data
    local sequences = {}
    local current_seq = nil
    
    for line in string.gmatch(fasta_data, "[^\r\n]+") do
        if line:sub(1, 1) == ">" then
            -- New sequence header
            current_seq = {}
            table.insert(sequences, current_seq)
        else
            -- Sequence data
            if current_seq then
                table.insert(current_seq, line)
            end
        end
    end
    
    -- Count symbols across all sequences
    for _, seq in ipairs(sequences) do
        local full_seq = table.concat(seq)
        for i = 1, #full_seq do
            local char = full_seq:sub(i, i)
            if symbol_count[char] then
                symbol_count[char] = symbol_count[char] + 1
            else
                symbol_count[char] = 1
            end
        end
    end
    
    -- Sort symbols alphabetically and return
    local symbols = {}
    for symbol in pairs(symbol_count) do
        table.insert(symbols, symbol)
    end
    table.sort(symbols)
    
    return symbols
end

-- Alternative simpler approach for the specific problem
function isolate_symbols_simple(input_lines)
    local symbol_count = {}
    
    -- Process each line (assuming sequences are given line by line)
    for _, line in ipairs(input_lines) do
        if line:sub(1, 1) ~= ">" then  -- Skip headers
            for i = 1, #line do
                local char = line:sub(i, i)
                if symbol_count[char] then
                    symbol_count[char] = symbol_count[char] + 1
                else
                    symbol_count[char] = 1
                end
            end
        end
    end
    
    -- Return sorted symbols
    local symbols = {}
    for symbol in pairs(symbol_count) do
        table.insert(symbols, symbol)
    end
    table.sort(symbols)
    
    return symbols
end

-- Example usage for Rosalind problem
function solve_isolating_symbols(input_data)
    -- Split input into lines
    local lines = {}
    for line in input_data:gmatch("[^\r\n]+") do
        table.insert(lines, line)
    end
    
    -- Process the alignment
    local symbol_count = {}
    
    for i, line in ipairs(lines) do
        if line:sub(1, 1) ~= ">" then  -- Skip FASTA headers
            for j = 1, #line do
                local char = line:sub(j, j)
                if symbol_count[char] then
                    symbol_count[char] = symbol_count[char] + 1
                else
                    symbol_count[char] = 1
                end
            end
        end
    end
    
    -- Extract and sort symbols
    local symbols = {}
    for symbol in pairs(symbol_count) do
        table.insert(symbols, symbol)
    end
    table.sort(symbols)
    
    return table.concat(symbols, " ")
end

-- Test with example data
local test_input = [[
>seq1
ACGTACGT
>seq2
TGCATGCA
>seq3
CGATCGAT]]

print("Solution:")
print(solve_isolating_symbols(test_input))
```

## Key Features of the Solution

1. **FASTA Parsing**: Handles FASTA format input with headers
2. **Symbol Counting**: Counts occurrences of each symbol across all sequences
3. **Alphabetical Sorting**: Returns symbols in alphabetical order
4. **Efficient Implementation**: Uses Lua's built-in string and table functions

## Expected Output Format

The output should be a space-separated list of symbols that appear in at least one sequence, sorted alphabetically.

## Example

For input sequences:
```
>seq1
ACGTACGT
>seq2
TGCATGCA
>seq3
CGATCGAT
```

The symbols A, C, G, T all appear in at least one sequence, so the output would be:
```
A C G T
```

This solution efficiently handles the alignment symbol isolation problem while maintaining clean, readable Lua code.

