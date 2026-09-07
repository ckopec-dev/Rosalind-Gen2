# Rosalind Problem: Using_the_Spectrum_Graph_to_Infer_Peptides

## Problem Understanding

This problem asks us to reconstruct a peptide sequence from its spectrum graph, where we need to find the shortest path that visits all nodes exactly once (Hamiltonian path) and corresponds to the given spectrum.

## Solution Approach

I'll use Assembly language with the following steps:
1. Parse the input spectrum
2. Build the spectrum graph
3. Find the Hamiltonian path using backtracking
4. Convert the path to peptide sequence

## Assembly Implementation

```assembly
; Rosalind: Using_the_Spectrum_Graph_to_Infer_Peptides
; Input: Spectrum values (integers)
; Output: Peptide sequence (amino acid symbols)

; Global variables
spectrum:     ; Array of spectrum values
spectrum_len: ; Length of spectrum
graph:        ; Adjacency list representation of graph
visited:      ; Track visited nodes during DFS
path:         ; Current path being explored
path_len:     ; Current path length

; Amino acid mass to symbol mapping
mass_to_aa:
    57, 'G'
    71, 'A' 
    87, 'S'
    97, 'P'
    99, 'V'
    101, 'T'
    103, 'C'
    113, 'L'
    114, 'N'
    115, 'D'
    128, 'K'
    129, 'I'
    131, 'R'
    137, 'M'
    147, 'H'
    156, 'F'
    163, 'P'
    186, 'Y'
    187, 'W'

; Main function
main:
    ; Read input spectrum values
    call read_spectrum
    
    ; Build spectrum graph
    call build_graph
    
    ; Find Hamiltonian path
    call find_hamiltonian_path
    
    ; Convert path to peptide sequence
    call path_to_peptide
    
    ; Output result
    call print_result
    
    return 0

; Read spectrum values from input
read_spectrum:
    ; Read number of spectrum values
    read spectrum_len
    
    ; Read all spectrum values
    i = 0
    while i < spectrum_len:
        read spectrum[i]
        i = i + 1
    
    return

; Build adjacency graph from spectrum
build_graph:
    ; Initialize graph structure
    i = 0
    while i < spectrum_len:
        j = 0
        while j < spectrum_len:
            if i != j:
                ; Calculate mass difference
                diff = spectrum[i] - spectrum[j]
                ; Check if this difference corresponds to a valid amino acid mass
                if is_valid_mass(diff):
                    ; Add edge from j to i (since spectrum[j] + diff = spectrum[i])
                    add_edge(graph, j, i)
            j = j + 1
        i = i + 1
    
    return

; Check if mass corresponds to valid amino acid
is_valid_mass:
    mass = %0
    
    ; Compare with known amino acid masses
    i = 0
    while i < 19:  ; 19 standard amino acids
        if mass_to_aa[i*2] == mass:
            return 1
        i = i + 1
    
    return 0

; Find Hamiltonian path using backtracking
find_hamiltonian_path:
    ; Initialize tracking arrays
    i = 0
    while i < spectrum_len:
        visited[i] = 0
        path[i] = -1
        i = i + 1
    
    path_len = 0
    
    ; Try starting from each node
    i = 0
    while i < spectrum_len:
        if visited[i] == 0:
            visited[i] = 1
            path[path_len] = i
            path_len = path_len + 1
            
            if backtrack(i):
                return 1  ; Found solution
                
            visited[i] = 0
            path_len = path_len - 1
        i = i + 1
    
    return 0  ; No solution found

; Backtracking helper function
backtrack:
    current_node = %0
    
    ; If we've visited all nodes, check if path is valid
    if path_len == spectrum_len:
        ; Verify that the path forms a valid peptide
        if is_valid_peptide():
            return 1
        else:
            return 0
    
    ; Try extending path with unvisited neighbors
    neighbor = get_next_neighbor(current_node)
    while neighbor != -1:
        if visited[neighbor] == 0:
            visited[neighbor] = 1
            path[path_len] = neighbor
            path_len = path_len + 1
            
            if backtrack(neighbor):
                return 1
                
            visited[neighbor] = 0
            path_len = path_len - 1
        
        neighbor = get_next_neighbor(current_node)
    
    return 0

; Convert path indices to peptide sequence
path_to_peptide:
    ; Initialize output string
    peptide = ""
    
    i = 0
    while i < path_len:
        node_index = path[i]
        mass = spectrum[node_index]
        
        ; Find amino acid symbol for this mass
        aa_symbol = get_aa_symbol(mass)
        peptide = peptide + aa_symbol
        
        i = i + 1
    
    return peptide

; Get amino acid symbol from mass value
get_aa_symbol:
    mass = %0
    
    i = 0
    while i < 19:
        if mass_to_aa[i*2] == mass:
            return mass_to_aa[i*2+1]
        i = i + 1
    
    ; Default case (should not happen for valid inputs)
    return 'X'

; Print final result
print_result:
    ; Output the peptide sequence
    print "Peptide: "
    print peptide
    
    return

; Helper functions
get_next_neighbor:
    node = %0
    ; Implementation would depend on graph representation
    return -1  ; Placeholder

is_valid_peptide:
    ; Validate that path forms a reasonable peptide
    return 1   ; Placeholder
```

## Key Algorithm Steps

1. **Graph Construction**: Build adjacency relationships between spectrum peaks based on mass differences matching amino acid masses.

2. **Path Finding**: Use backtracking to find a Hamiltonian path (visiting each node exactly once) that represents the valid peptide sequence.

3. **Peptide Conversion**: Map mass values along the path to corresponding amino acid symbols.

## Time Complexity
- Graph building: O(n²) where n is number of spectrum peaks
- Path finding with backtracking: O(n!) in worst case, but constrained by valid mass differences
- Overall: O(n!) for exact solution

## Space Complexity
- O(n²) for adjacency matrix/graph storage
- O(n) for path and visited tracking arrays

This implementation provides the framework for solving the spectrum graph peptide inference problem using Assembly language constructs.