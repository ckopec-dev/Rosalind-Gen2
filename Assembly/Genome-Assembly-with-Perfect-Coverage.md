# Rosalind Problem: Genome Assembly with Perfect Coverage

## Problem Understanding

In this problem, we need to reconstruct a genome from a set of reads that perfectly cover the genome (each position is covered exactly once). This is essentially finding a Eulerian path in a de Bruijn graph.

## Solution Approach

1. **Build De Bruijn Graph**: Create a graph where each k-mer becomes a node, and edges represent overlaps
2. **Find Eulerian Path**: Use Hierholzer's algorithm to find the path that visits every edge exactly once
3. **Reconstruct Genome**: Follow the path to reconstruct the original genome

## Assembly Code Solution

```assembly
; Genome Assembly with Perfect Coverage
; Input: Set of k-mers (reads) that perfectly cover a genome
; Output: The reconstructed genome sequence

; Constants
K = 12          ; k-mer length
MAX_READS = 1000
MAX_NODES = 5000

; Data structures
read_buffer:    .space 10000      ; Buffer for input reads
graph_nodes:    .space MAX_NODES * 2  ; Node pairs (in, out)
edges:          .space MAX_NODES * 2  ; Edge list storage
visited:        .space MAX_NODES      ; Visited flags
path:           .space MAX_NODES * 2  ; Result path
stack:          .space MAX_NODES * 2  ; DFS stack

; Global variables
num_reads:      .word 0
num_nodes:      .word 0
node_count:     .word 0
path_len:       .word 0

main:
    ; Initialize data structures
    mov r0, #0
    mov num_reads, r0
    mov num_nodes, r0
    mov node_count, r0
    mov path_len, r0
    
    ; Read input reads
    call read_reads
    
    ; Build de Bruijn graph
    call build_graph
    
    ; Find Eulerian path
    call find_eulerian_path
    
    ; Reconstruct genome
    call reconstruct_genome
    
    ; Output result
    call print_result
    
    halt

; Read input reads from stdin
read_reads:
    mov r1, #read_buffer
    mov r2, #0        ; read count
    
read_loop:
    ; Read one line
    call get_line
    
    ; Check if end of input
    cmp r0, #0
    beq read_done
    
    ; Store read
    str r0, [r1, r2, lsl #2]
    
    add r2, r2, #1
    add r1, r1, #4
    
    b read_loop
    
read_done:
    mov num_reads, r2
    bx lr

; Build de Bruijn graph from reads
build_graph:
    mov r0, #0        ; read index
    
build_loop:
    cmp r0, num_reads
    bge build_done
    
    ; Get current read
    ldr r1, [read_buffer, r0, lsl #2]
    
    ; Extract k-mers from read
    mov r2, #0        ; k-mer index
    
kmer_loop:
    cmp r2, K
    bge kmer_done
    
    ; Extract k-mer of length K from position r2
    call extract_kmer
    
    ; Add to graph
    call add_to_graph
    
    add r2, r2, #1
    b kmer_loop
    
kmer_done:
    add r0, r0, #1
    b build_loop
    
build_done:
    bx lr

; Extract k-mer from read at position pos
extract_kmer:
    ; r1 = read pointer, r2 = position
    mov r3, r1
    add r3, r3, r2        ; Point to start of k-mer
    mov r4, #K
    
kmer_copy_loop:
    cmp r4, #0
    beq kmer_copy_done
    
    ldrb r5, [r3, r4, sub #1]
    ; Store in temporary buffer
    strb r5, [temp_buffer, r4, sub #1]
    
    sub r4, r4, #1
    b kmer_copy_loop
    
kmer_copy_done:
    bx lr

; Add k-mer to de Bruijn graph
add_to_graph:
    ; r1 = k-mer pointer (already in temp_buffer)
    ; Extract prefix and suffix for graph edges
    
    ; Get prefix (first K-1 bases)
    call get_prefix
    
    ; Get suffix (last K-1 bases)
    call get_suffix
    
    ; Create edge from prefix to suffix
    call create_edge
    
    bx lr

; Get prefix of k-mer (first K-1 bases)
get_prefix:
    mov r3, #0
    mov r4, #K
    
prefix_loop:
    cmp r3, K-1
    bge prefix_done
    
    ldrb r5, [temp_buffer, r3]
    strb r5, [prefix_buffer, r3]
    
    add r3, r3, #1
    b prefix_loop
    
prefix_done:
    bx lr

; Get suffix of k-mer (last K-1 bases)
get_suffix:
    mov r3, #0
    mov r4, #K
    
suffix_loop:
    cmp r3, K-1
    bge suffix_done
    
    ldrb r5, [temp_buffer, r3, add K-1]
    strb r5, [suffix_buffer, r3]
    
    add r3, r3, #1
    b suffix_loop
    
suffix_done:
    bx lr

; Create edge in graph
create_edge:
    ; r1 = prefix, r2 = suffix
    ; Find or create nodes for prefix and suffix
    
    ; Find prefix node
    call find_or_create_node
    
    ; Find suffix node  
    call find_or_create_node
    
    ; Add edge from prefix to suffix
    call add_edge
    
    bx lr

; Find or create node in graph
find_or_create_node:
    ; r1 = node identifier (k-mer)
    mov r2, #0        ; node index
    
node_search_loop:
    cmp r2, num_nodes
    bge node_not_found
    
    ; Compare with existing node
    ldr r3, [graph_nodes, r2, lsl #2]
    
    ; Check if same as current node (simplified)
    cmp r3, r1
    beq node_found
    
    add r2, r2, #1
    b node_search_loop
    
node_not_found:
    ; Create new node
    str r1, [graph_nodes, num_nodes, lsl #2]
    add num_nodes, num_nodes, #1
    bx lr
    
node_found:
    bx lr

; Add edge between two nodes
add_edge:
    ; r1 = from_node, r2 = to_node
    mov r3, node_count
    
    ; Store edge
    str r1, [edges, r3, lsl #2]        ; From
    str r2, [edges, r3, lsl #2, add #4] ; To
    
    add node_count, node_count, #1
    bx lr

; Find Eulerian path using Hierholzer's algorithm
find_eulerian_path:
    ; Initialize visited array
    mov r0, #0
    mov r1, #0
    
visited_loop:
    cmp r1, num_nodes
    bge visited_done
    
    strb r0, [visited, r1]
    add r1, r1, #1
    b visited_loop
    
visited_done:
    
    ; Find starting node (node with out-degree > 0)
    mov r0, #0
    mov start_node, r0
    
    ; Push starting node to stack
    call push_stack
    
    ; Process until stack is empty
    mov r1, #0
    
stack_loop:
    cmp r1, #0        ; Check if stack is empty
    beq stack_done
    
    ; Pop from stack
    call pop_stack
    
    ; Get current node
    mov r2, r0
    
    ; While current node has unvisited edges
    mov r3, #0        ; edge index
    
edge_loop:
    cmp r3, node_count
    bge edge_loop_done
    
    ; Check if edge is from current node
    ldr r4, [edges, r3, lsl #2]
    cmp r4, r2
    bne edge_loop_continue
    
    ; Check if edge is visited
    ldrb r5, [visited, r3]
    cmp r5, #1
    beq edge_loop_continue
    
    ; Mark as visited
    mov r5, #1
    strb r5, [visited, r3]
    
    ; Push destination node to stack
    ldr r4, [edges, r3, lsl #2, add #4]
    call push_stack
    
    b edge_loop
    
edge_loop_continue:
    add r3, r3, #1
    b edge_loop
    
edge_loop_done:
    ; Add current node to path
    str r2, [path, path_len, lsl #2]
    add path_len, path_len, #1
    
    b stack_loop
    
stack_done:
    bx lr

; Push element to stack
push_stack:
    mov r1, stack_ptr
    str r0, [r1]
    add stack_ptr, stack_ptr, #4
    bx lr

; Pop element from stack
pop_stack:
    sub stack_ptr, stack_ptr, #4
    ldr r0, [stack_ptr]
    bx lr

; Reconstruct genome from Eulerian path
reconstruct_genome:
    mov r0, #0        ; path index
    
genome_loop:
    cmp r0, path_len
    bge genome_done
    
    ; Get node from path
    ldr r1, [path, r0, lsl #2]
    
    ; Add first base of node to result
    if r0 == 0:
        ; First node - add all bases
        mov r2, #0
        add_genome_base_loop:
            ldrb r3, [node_buffer, r2]
            strb r3, [genome_result, r4]
            add r2, r2, #1
            add r4, r4, #1
            cmp r2, K-1
            blt add_genome_base_loop
    else:
        ; Subsequent nodes - add only last base
        ldrb r3, [node_buffer, K-1]
        strb r3, [genome_result, r4]
        add r4, r4, #1
    
    add r0, r0, #1
    b genome_loop
    
genome_done:
    bx lr

; Print final result
print_result:
    mov r0, #1        ; stdout
    mov r1, #genome_result
    mov r2, #genome_len
    call write_string
    
    bx lr

; Helper functions for I/O
get_line:
    ; Read line from stdin (simplified)
    bx lr

write_string:
    ; Write string to stdout (simplified)  
    bx lr
```

## Explanation of Key Components

### 1. **Graph Construction**
- Convert each k-mer into a de Bruijn graph where nodes are (k-1)-mers and edges represent overlaps
- Each k-mer contributes two nodes: prefix and suffix

### 2. **Eulerian Path Finding**
- Uses Hierholzer's algorithm to find the path that visits every edge exactly once
- Maintains a stack to track the current traversal path

### 3. **Genome Reconstruction**
- Follows the Eulerian path to reconstruct the original genome sequence
- The first k-mer contributes all bases, subsequent k-mers contribute only their last base

## Time Complexity
- **Graph construction**: O(n×k) where n is number of reads and k is k-mer length
- **Eulerian path finding**: O(E) where E is number of edges
- **Overall**: O(n×k)

This solution handles the perfect coverage assumption, where each genome position is covered exactly once by the given reads.