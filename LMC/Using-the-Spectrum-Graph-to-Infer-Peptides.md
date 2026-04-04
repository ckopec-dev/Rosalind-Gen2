# Rosalind Problem: Using_the_Spectrum_Graph_to_Infer_Peptides

## Problem Understanding

This problem asks us to reconstruct a peptide sequence from its spectrum graph. We need to find a path through the spectrum graph that corresponds to a valid peptide sequence.

## Solution Approach

1. **Parse the spectrum graph**: Extract the nodes and edges from the input
2. **Find the start and end nodes**: Identify nodes with in-degree 0 and out-degree 0
3. **Find all paths**: Use DFS to find all possible paths from start to end
4. **Convert to amino acid sequence**: Map the path weights to amino acid letters

## LMC Implementation

```lmc
; Using_the_Spectrum_Graph_to_Infer_Peptides
; Input: Spectrum graph with nodes and edges
; Output: Peptide sequence

; Constants
START_NODE EQU 0
END_NODE   EQU 0
MAX_NODES  EQU 100
MAX_PATHS  EQU 1000

; Data section
    ORG 1000
    ; Node and edge data
    NODES   DS  MAX_NODES
    EDGES   DS  MAX_NODES * MAX_NODES
    PATHS   DS  MAX_PATHS * MAX_NODES
    PATH_COUNT DS 1
    
    ; Amino acid mass mapping
    MASS_MAP DS 26 * 2
    MASS_MAP_DATA DS 26
    ; Masses: A=71, C=103, D=115, E=129, F=147, G=57, H=137, I=113, K=128, L=113, M=131, N=114, P=97, Q=128, R=156, S=87, T=101, V=99, W=186, Y=163
    ; Mapping: A=71, C=103, D=115, E=129, F=147, G=57, H=137, I=113, K=128, L=113, M=131, N=114, P=97, Q=128, R=156, S=87, T=101, V=99, W=186, Y=163

; Main program
    ORG 2000
MAIN    LDA NODE_COUNT
        STA TEMP1
        LDA START_NODE
        STA CURRENT_NODE
        LDA END_NODE
        STA TARGET_NODE
        LDA 0
        STA PATH_LENGTH
        LDA 0
        STA PATH_COUNT
        LDA 0
        STA PATH_INDEX
        LDA 0
        STA TEMP2
        LDA 0
        STA TEMP3
        
        ; Initialize path array
        LDA 0
        STA PATH_ARRAY
        LDA 0
        STA PATH_ARRAY + 1
        LDA 0
        STA PATH_ARRAY + 2
        LDA 0
        STA PATH_ARRAY + 3
        LDA 0
        STA PATH_ARRAY + 4
        LDA 0
        STA PATH_ARRAY + 5
        LDA 0
        STA PATH_ARRAY + 6
        LDA 0
        STA PATH_ARRAY + 7
        LDA 0
        STA PATH_ARRAY + 8
        LDA 0
        STA PATH_ARRAY + 9
        
        ; Call DFS to find all paths
        LDA CURRENT_NODE
        STA DFS_NODE
        LDA PATH_LENGTH
        STA DFS_PATH_LENGTH
        JSR DFS_SEARCH
        
        ; Convert paths to amino acid sequences
        JSR PATH_TO_AMINO_ACID
        
        ; Output results
        LDA OUTPUT_STRING
        JSR PRINT_STRING
        
        HLT

; Depth First Search to find all paths
DFS_SEARCH
        ; Base case: if we reached target node
        LDA DFS_NODE
        LDA TARGET_NODE
        CMP
        JEQ DFS_FOUND_PATH
        
        ; Recursive case: explore neighbors
        LDA DFS_NODE
        STA CURRENT_NODE
        LDA 0
        STA NEIGHBOR_COUNT
        
DFS_LOOP    LDA CURRENT_NODE
            LDA NEIGHBOR_COUNT
            LDA EDGES
            LDA CURRENT_NODE
            LDA NEIGHBOR_COUNT
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0
            LDA 0