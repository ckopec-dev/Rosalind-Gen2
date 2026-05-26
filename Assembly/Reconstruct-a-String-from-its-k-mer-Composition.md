# Rosalind Problem: Reconstruct a String from its k-mer Composition

## Problem Understanding

Given a collection of k-mers, we need to reconstruct the original DNA string. This is a classic de Bruijn graph problem where we need to find an Eulerian path in the de Bruijn graph.

## Approach

1. **Build de Bruijn graph**: Each k-mer becomes a node, and we connect nodes with edges representing overlaps
2. **Find Eulerian path**: Use Hierholzer's algorithm to find a path that visits every edge exactly once
3. **Reconstruct string**: Follow the path and concatenate the appropriate parts

## Assembly Implementation

```assembly
; Reconstruct a string from k-mer composition
; Input: list of k-mers
; Output: reconstructed string

; Constants
MAX_KMERS EQU 1000
MAX_KMER_LEN EQU 20
MAX_NODES EQU 1000

; Data section
data_segment
    kmers db MAX_KMERS * MAX_KMER_LEN dup(0)  ; Store k-mers
    kmer_count dw 0                           ; Number of k-mers
    kmer_len dw 0                             ; Length of each k-mer
    graph_edges dd MAX_NODES * 10 dup(0)      ; Adjacency list for graph
    edge_count dd MAX_NODES dup(0)            ; Number of edges per node
    in_degree dd MAX_NODES dup(0)             ; In-degree of each node
    out_degree dd MAX_NODES dup(0)            ; Out-degree of each node
    visited dd MAX_NODES dup(0)               ; Visited flag for nodes
    euler_path dd MAX_NODES dup(0)            ; Store Eulerian path
    path_len dd 0                             ; Length of Eulerian path
    node_map dd MAX_NODES dup(0)              ; Map k-mers to node IDs

code_segment
main:
    ; Initialize data structures
    call initialize_globals
    
    ; Read input k-mers
    call read_kmers
    
    ; Build de Bruijn graph
    call build_graph
    
    ; Find Eulerian path
    call find_eulerian_path
    
    ; Reconstruct original string
    call reconstruct_string
    
    ; Output result
    call output_result
    
    ; Exit program
    mov eax, 1
    int 0x80

initialize_globals:
    ; Initialize all arrays to zero
    xor ecx, ecx
    mov edi, offset kmers
    mov ecx, MAX_KMERS * MAX_KMER_LEN
    xor eax, eax
    rep stosb
    
    mov edi, offset graph_edges
    mov ecx, MAX_NODES * 10
    xor eax, eax
    rep stosb
    
    mov edi, offset edge_count
    mov ecx, MAX_NODES
    xor eax, eax
    rep stosb
    
    mov edi, offset in_degree
    mov ecx, MAX_NODES
    xor eax, eax
    rep stosb
    
    mov edi, offset out_degree
    mov ecx, MAX_NODES
    xor eax, eax
    rep stosb
    
    mov edi, offset visited
    mov ecx, MAX_NODES
    xor eax, eax
    rep stosb
    
    mov edi, offset euler_path
    mov ecx, MAX_NODES
    xor eax, eax
    rep stosb
    
    mov dword ptr [kmer_count], 0
    mov dword ptr [path_len], 0
    ret

read_kmers:
    ; Read k-mers from input (simplified)
    ; In practice, this would read from stdin or file
    ; For this example, we'll assume kmers are already loaded
    
    ; Set k-mer length (example: 4)
    mov word ptr [kmer_len], 4
    
    ; Set number of k-mers (example: 6)
    mov word ptr [kmer_count], 6
    
    ; Example k-mers: ACGT, CGTA, GTAC, TACG, ACCT, CCTG
    ; This would be replaced with actual input reading
    
    ret

build_graph:
    ; Build de Bruijn graph from k-mers
    ; For each k-mer, create prefix and suffix
    ; Connect prefix to suffix with an edge
    
    mov esi, 0                          ; k-mer index
    mov ecx, [kmer_count]               ; Total k-mers
    
build_loop:
    ; Check if we're done
    cmp esi, ecx
    jge build_done
    
    ; Get current k-mer
    mov eax, esi
    imul eax, MAX_KMER_LEN
    mov edi, offset kmers
    add edi, eax
    
    ; Extract prefix (k-1 characters)
    mov ebx, [kmer_len]
    dec ebx                             ; k-1
    mov edx, ebx
    mov ecx, ebx
    
    ; Get prefix string
    mov edi, offset prefix
    mov esi, offset kmers
    add esi, eax
    mov ecx, ebx
    rep movsb
    
    ; Extract suffix (k-1 characters)
    mov ebx, [kmer_len]
    dec ebx
    mov ecx, ebx
    mov esi, offset kmers
    add esi, eax
    add esi, 1                          ; Skip first character
    mov edi, offset suffix
    rep movsb
    
    ; Convert prefix and suffix to node IDs
    call hash_string
    mov eax, [hash_result]
    mov ebx, [node_id]
    
    ; Add edge from prefix to suffix
    call add_edge
    
    inc esi                             ; Next k-mer
    jmp build_loop
    
build_done:
    ret

add_edge:
    ; Add edge from node u to node v
    ; graph_edges[u][out_degree[u]] = v
    ; out_degree[u]++
    
    mov eax, [node_u]                   ; Get source node
    mov ebx, [node_v]                   ; Get destination node
    
    ; Get current edge count for source node
    mov ecx, [out_degree + eax * 4]
    
    ; Add edge
    mov dword ptr [graph_edges + eax * 4 * 10 + ecx * 4], ebx
    
    ; Increment edge count
    inc dword ptr [out_degree + eax * 4]
    
    ; Increment in-degree of destination
    inc dword ptr [in_degree + ebx * 4]
    
    ret

find_eulerian_path:
    ; Find Eulerian path using Hierholzer's algorithm
    
    ; Find starting node (node with out-degree > in-degree)
    mov esi, 0
    mov ecx, MAX_NODES
    mov eax, 0
    
find_start_loop:
    cmp esi, ecx
    jge find_start_done
    
    mov ebx, [out_degree + esi * 4]
    mov edx, [in_degree + esi * 4]
    cmp ebx, edx
    jg find_start_found
    
    inc esi
    jmp find_start_loop
    
find_start_found:
    mov eax, esi                        ; Starting node
    mov ebx, eax
    
find_start_done:
    ; Start DFS from starting node
    call dfs_eulerian
    ret

dfs_eulerian:
    ; DFS to find Eulerian path
    ; This is a simplified version
    
    ; Push current node to path
    mov ecx, [path_len]
    mov dword ptr [euler_path + ecx * 4], eax
    inc dword ptr [path_len]
    
    ; Get current node's edges
    mov esi, eax                        ; Current node
    mov ecx, [out_degree + esi * 4]     ; Number of edges
    
    ; For each edge
    xor edi, edi
dfs_edge_loop:
    cmp edi, ecx
    jge dfs_edge_done
    
    ; Get next node
    mov ebx, [graph_edges + esi * 4 * 10 + edi * 4]
    
    ; Remove edge (mark as visited)
    mov dword ptr [visited + ebx * 4], 1
    
    ; Recursively visit next node
    push ebx
    call dfs_eulerian
    pop ebx
    
    inc edi
    jmp dfs_edge_loop
    
dfs_edge_done:
    ret

reconstruct_string:
    ; Reconstruct original string from Eulerian path
    ; First k-mer is first node in path
    ; Subsequent k-mers are overlapped
    
    mov esi, 0
    mov ecx, [path_len]
    mov eax, [kmer_len]
    dec eax                             ; k-1 for overlap
    
    ; First k-mer
    mov ebx, [euler_path]
    mov edi, offset reconstructed
    mov edx, eax                        ; k-1 characters
    
    ; Copy first k-mer to result
    mov esi, ebx
    imul esi, MAX_KMER_LEN
    mov esi, offset kmers
    add esi, esi
    mov ecx, [kmer_len]
    rep movsb
    
    ; Add remaining characters from path
    mov esi, 1                          ; Start from second node
    mov ecx, [path_len]
    dec ecx                             ; Skip first node
    
reconstruct_loop:
    cmp esi, ecx
    jge reconstruct_done
    
    ; Get current node
    mov ebx, [euler_path + esi * 4]
    
    ; Get k-mer
    mov edi, offset kmers
    imul ebx, MAX_KMER_LEN
    add edi, ebx
    
    ; Add last character of k-mer to result
    mov ecx, [kmer_len]
    dec ecx                             ; Last character position
    mov al, [edi + ecx]
    mov [reconstructed + eax], al
    
    inc esi
    jmp reconstruct_loop
    
reconstruct_done:
    ret

output_result:
    ; Output reconstructed string
    mov eax, 4                          ; sys_write
    mov ebx, 1                          ; stdout
    mov ecx, offset reconstructed
    mov edx, [kmer_len]
    add edx, [path_len]
    sub edx, 1                          ; Adjust for overlap
    int 0x80
    
    ; Print newline
    mov eax, 4
    mov ebx, 1
    mov ecx, offset newline
    mov edx, 1
    int 0x80
    
    ret

; Helper functions
hash_string:
    ; Simple hash function for k-mer string
    ; Convert k-mer to numeric ID
    
    mov esi, offset prefix
    mov ecx, [kmer_len]
    dec ecx                             ; k-1 characters
    
    xor eax, eax                        ; hash result
    mov ebx, 31                         ; prime number
    
hash_loop:
    cmp ecx, 0
    jz hash_done
    
    movzx edx, byte ptr [esi]
    imul eax, ebx
    add eax, edx
    
    inc esi
    dec ecx
    jmp hash_loop
    
hash_done:
    mov [hash_result], eax
    ret

; Data sections
prefix db 20 dup(0)
suffix db 20 dup(0)
reconstructed db 1000 dup(0)
newline db 10
hash_result dd 0
node_id dd 0
node_u dd 0
node_v dd 0
```

## Key Algorithm Steps

1. **Graph Construction**: For each k-mer, create a de Bruijn graph where:
   - Nodes represent (k-1)-mers
   - Edges represent k-mers connecting prefixes to suffixes

2. **Eulerian Path Finding**: Use Hierholzer's algorithm:
   - Find starting node (out-degree > in-degree)
   - Perform DFS to find Eulerian path
   - Backtrack and build path

3. **String Reconstruction**: 
   - Start with first k-mer
   - Add one character at a time from subsequent k-mers
   - Account for overlaps between consecutive k-mers

## Time Complexity
- Graph construction: O(n×k) where n is number of k-mers and k is k-mer length
- Eulerian path finding: O(n)
- Overall: O(n×k)

## Space Complexity
- O(n×k) for storing k-mers and graph structure

This assembly implementation provides the framework for solving the k-mer reconstruction problem using de Bruijn graphs and Eulerian path algorithms.

