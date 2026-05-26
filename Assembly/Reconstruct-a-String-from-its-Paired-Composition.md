# Rosalind Problem: Reconstruct a String from its Paired Composition

## Problem Understanding

Given a paired composition of a string (k-mers with their corresponding l-mer pairs), we need to reconstruct the original string. This is a variant of the Eulerian path problem in de Bruijn graphs.

## Approach

1. **Build De Bruijn Graph**: Create a graph where nodes are (k-1)-mers and edges represent k-mers with their paired structure
2. **Find Eulerian Path**: Use Hierholzer's algorithm to find the path that visits each edge exactly once
3. **Reconstruct String**: Follow the path to build the original string

## Assembly Implementation

```assembly
; Function to reconstruct string from paired composition
; Input: paired_composition (list of (k-mer, l-mer) pairs)
; Output: reconstructed string

; Global variables
MAX_NODES equ 1000
MAX_EDGES equ 2000
NODES db MAX_NODES dup(?)
EDGES db MAX_EDGES dup(?)
DEGREE db MAX_NODES dup(?)
PATH db 1000 dup(?)

; Main function
reconstruct_string:
    ; Initialize data structures
    call initialize_graph
    
    ; Build the de Bruijn graph from paired composition
    call build_graph_from_pairs
    
    ; Find Eulerian path
    call find_eulerian_path
    
    ; Reconstruct the string from path
    call reconstruct_from_path
    
    ret

; Build graph from paired composition
build_graph_from_pairs:
    ; For each pair (k-mer, l-mer) in input:
    ; - Create nodes for prefix and suffix of k-mer
    ; - Create edges connecting them with l-mer information
    push bp
    mov bp, sp
    
    ; Get input parameters
    mov si, [bp+6]  ; paired_composition pointer
    
    ; Process each pair
    mov cx, 0
    mov di, 0
    
process_pairs:
    ; Check if we've processed all pairs
    cmp cx, [bp+8]  ; number of pairs
    jge pairs_done
    
    ; Get k-mer and l-mer from current pair
    mov ax, [si]    ; k-mer
    mov bx, [si+2]  ; l-mer
    
    ; Extract prefix and suffix of k-mer
    call extract_prefix_suffix
    
    ; Create nodes and edges in graph
    call create_node_edge
    
    ; Move to next pair
    add si, 4       ; advance by 2 words
    inc cx
    jmp process_pairs
    
pairs_done:
    pop bp
    ret

; Extract prefix and suffix from k-mer
extract_prefix_suffix:
    push bp
    mov bp, sp
    
    ; Assume k-mer is stored in ax
    ; Extract (k-1) prefix and suffix
    mov bx, ax
    shr bx, 1       ; shift right to get prefix
    mov cx, ax
    and cx, 0FFh    ; mask to get suffix
    
    ; Store prefix in bx, suffix in cx
    ; (implementation depends on actual k-mer encoding)
    
    pop bp
    ret

; Create node and edge in graph
create_node_edge:
    push bp
    mov bp, sp
    
    ; Create nodes for prefix and suffix
    ; Add edge from prefix to suffix with l-mer info
    ; Implementation depends on graph representation
    
    pop bp
    ret

; Find Eulerian path using Hierholzer's algorithm
find_eulerian_path:
    push bp
    mov bp, sp
    
    ; Find starting node (node with out-degree - in-degree = 1)
    call find_starting_node
    
    ; Initialize stack with starting node
    push ax
    
    ; While stack is not empty:
    ;   Pop node from stack
    ;   While node has unvisited edges:
    ;     Follow edge, mark as visited, push to stack
    ;   Add node to path
    
    pop bp
    ret

; Reconstruct string from Eulerian path
reconstruct_from_path:
    push bp
    mov bp, sp
    
    ; Follow path and build string
    ; Use overlap of k-1-mers to reconstruct original string
    ; Handle paired structure appropriately
    
    pop bp
    ret

; Helper function to initialize graph
initialize_graph:
    push bp
    mov bp, sp
    
    ; Initialize nodes array
    mov di, 0
    mov cx, MAX_NODES
init_loop:
    mov NODES[di], 0
    add di, 1
    loop init_loop
    
    ; Initialize degree array
    mov di, 0
    mov cx, MAX_NODES
deg_loop:
    mov DEGREE[di], 0
    add di, 1
    loop deg_loop
    
    pop bp
    ret

; Function to get next unvisited edge
get_next_edge:
    push bp
    mov bp, sp
    
    ; Find first unvisited edge from current node
    ; Return edge information or -1 if none
    
    pop bp
    ret

; Function to mark edge as visited
mark_edge_visited:
    push bp
    mov bp, sp
    
    ; Mark specified edge as visited
    
    pop bp
    ret
```

## Key Algorithm Steps

1. **Graph Construction**: 
   - Each k-mer contributes to a node in the de Bruijn graph
   - Edges represent transitions between overlapping k-mers
   - Store paired information in edge labels

2. **Eulerian Path Finding**:
   - Use Hierholzer's algorithm with stack-based approach
   - Start from node with out-degree - in-degree = 1
   - Follow unvisited edges until returning to start

3. **String Reconstruction**:
   - Trace the Eulerian path
   - Use k-1-mer overlaps to build original sequence
   - Handle paired composition constraints appropriately

## Time Complexity
- **Graph Construction**: O(n) where n is number of pairs
- **Eulerian Path**: O(E) where E is number of edges  
- **Overall**: O(n + E) = O(n)

## Space Complexity
- O(V + E) where V is number of nodes and E is number of edges

This implementation provides the framework for solving the paired de Bruijn graph reconstruction problem using assembly language concepts.

