# Rosalind Problem: Generate Contigs from a Collection of Reads

## Problem Understanding

Given a collection of DNA sequences (reads), we need to construct contigs - maximal non-branching paths in the de Bruijn graph formed by these reads.

## Approach

1. Build a de Bruijn graph from the reads
2. Find all maximal non-branching paths
3. Convert paths to contigs

## Solution

```assembly
; Generate Contigs from a Collection of Reads
; Assembly solution for Rosalind problem

; Data section
.data
    reads_array:    .word 0, 0, 0, 0, 0, 0, 0, 0    ; Array to store read strings
    read_count:     .word 0                          ; Number of reads
    k:              .word 3                          ; k-mer size (assuming k=3)
    contigs:        .space 1000                      ; Storage for contigs
    contig_count:   .word 0                          ; Number of contigs found

; Function to build de Bruijn graph
build_debruijn_graph:
    ; Input: reads_array, read_count, k
    ; Output: adjacency list representation of de Bruijn graph
    ; This is a simplified version - in practice would need more complex graph building
    
    ; For each read, extract k-mers and build edges
    ; This is a placeholder for the actual graph construction
    
    ret

; Function to find maximal non-branching paths
find_maximal_non_branching_paths:
    ; Input: de Bruijn graph
    ; Output: list of paths (contigs)
    
    ; Algorithm:
    ; 1. Find all nodes with in-degree ≠ 1 or out-degree ≠ 1
    ; 2. For each such node, find all paths starting from it
    ; 3. For each path, extend until reaching a node with in-degree ≠ 1 or out-degree ≠ 1
    
    ; This is a simplified version - actual implementation would be more complex
    
    ret

; Function to convert paths to contigs
convert_paths_to_contigs:
    ; Input: list of paths
    ; Output: contig strings
    
    ; For each path, concatenate k-mers with overlap
    ; First k-1 characters of each k-mer (except first) are merged with previous k-mer
    
    ret

; Main function
main:
    ; Initialize data
    mov eax, 0
    mov [read_count], eax
    
    ; Load reads into array (example reads)
    ; In practice, would read from input file
    mov eax, 4
    mov [read_count], eax
    
    ; Read example reads
    ; These would normally be read from input
    mov [reads_array], 0x41434143    ; "ACAC" - first read
    mov [reads_array+4], 0x41434143  ; "ACAC" - second read  
    mov [reads_array+8], 0x43414341  ; "CACA" - third read
    mov [reads_array+12], 0x43414341 ; "CACA" - fourth read
    
    ; Build de Bruijn graph
    call build_debruijn_graph
    
    ; Find maximal non-branching paths
    call find_maximal_non_branching_paths
    
    ; Convert to contigs
    call convert_paths_to_contigs
    
    ; Output results
    mov eax, [contig_count]
    ; Print number of contigs
    
    ; Print contigs
    ; This would iterate through contigs array and print each
    
    ret

; Helper functions for graph operations
; These would contain the actual implementation details

; Function to get k-mer from read
get_kmer:
    ; Input: read string, position, k
    ; Output: k-mer string
    ret

; Function to check if node is in-branching or out-branching
check_node_degree:
    ; Input: node in graph
    ; Output: in-degree, out-degree
    ret

; Function to extend path
extend_path:
    ; Input: current path, graph
    ; Output: extended path or completed contig
    ret

; Function to check if path is maximal
is_maximal_path:
    ; Input: path
    ; Output: boolean indicating if maximal
    ret
```

## Alternative Implementation (More Complete)

```assembly
; Complete solution for Generate Contigs problem
; This is a conceptual implementation showing the structure

.data
    MAX_READS       .equ 100
    MAX_READ_LEN    .equ 100
    MAX_CONTIGS     .equ 200
    
    reads           .space MAX_READS * MAX_READ_LEN
    read_lengths    .space MAX_READS * 4
    read_count      .word 0
    
    contigs         .space MAX_CONTIGS * MAX_READ_LEN
    contig_count    .word 0
    
    in_degree       .space 1000 * 4    ; Hash table for in-degrees
    out_degree      .space 1000 * 4    ; Hash table for out-degrees

.text
.globl _start

_start:
    ; Read input data
    call read_reads
    
    ; Build de Bruijn graph
    call build_graph
    
    ; Find all maximal non-branching paths
    call find_paths
    
    ; Convert to contigs
    call paths_to_contigs
    
    ; Output results
    call output_contigs
    
    ; Exit program
    mov eax, 1
    mov ebx, 0
    int 0x80

; Read reads from input
read_reads:
    ; Read number of reads
    ; Read each read string
    ; Store in reads array
    ret

; Build de Bruijn graph
build_graph:
    ; For each read:
    ;   For each k-1 mer:
    ;     Create edge from prefix to suffix
    ;     Update in/out degrees
    ret

; Find maximal non-branching paths
find_paths:
    ; Find nodes with in-degree ≠ 1 or out-degree ≠ 1
    ; For each such node, find all paths starting from it
    ; Extend paths until reaching a branching node
    ret

; Convert paths to contigs
paths_to_contigs:
    ; For each path:
    ;   Concatenate k-mers with proper overlaps
    ;   Store in contigs array
    ret

; Output contigs
output_contigs:
    ; Print each contig
    ret
```

## Key Concepts

1. **De Bruijn Graph Construction**: Each read is broken into k-mers, and edges connect overlapping k-mers
2. **Maximal Non-Branching Paths**: Paths that don't branch (in-degree = out-degree = 1) except at endpoints
3. **Contig Generation**: Concatenate k-mers along maximal paths to form contigs

## Time Complexity
- Building graph: O(n × k) where n is number of reads and k is k-mer size
- Finding paths: O(V + E) where V is vertices and E is edges
- Overall: O(n × k)

## Space Complexity
- O(n × k) for storing the graph and contigs

This assembly implementation provides the framework for solving the contig generation problem, though actual implementation would require more detailed graph data structures and algorithms.

