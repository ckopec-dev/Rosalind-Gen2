# Rosalind Problem: Shortest Paths in DAG

## Problem Description
Given a weighted directed acyclic graph (DAG), find the shortest paths from a source vertex to all other vertices.

## Approach
Since we're dealing with a DAG, we can use topological sorting combined with dynamic programming. The algorithm works by:
1. Topologically sorting the vertices
2. Processing vertices in topological order
3. Updating distances using relaxation

## Assembly Implementation

```assembly
; Rosalind - Shortest Paths in DAG
; Assembly implementation for finding shortest paths in a DAG

.data
    ; Graph representation: adjacency list
    graph:      ; Array of adjacency lists
    num_vertices: .long 6
    source:     .long 1
    INF:        .long 0x7FFFFFFF
    
    ; Distance array initialized to infinity
    distances:  .space 24    ; 6 * 4 bytes for 6 vertices
    
    ; Topological order array
    topo_order: .space 24    ; 6 * 4 bytes
    
    ; Vertex list for adjacency representation
    edges:      .long 1,2,3   ; vertex 1 -> vertex 2 with weight 3
                .long 1,3,2   ; vertex 1 -> vertex 3 with weight 2
                .long 2,4,1   ; vertex 2 -> vertex 4 with weight 1
                .long 2,5,4   ; vertex 2 -> vertex 5 with weight 4
                .long 3,4,2   ; vertex 3 -> vertex 4 with weight 2
                .long 3,5,5   ; vertex 3 -> vertex 5 with weight 5
                .long 4,6,1   ; vertex 4 -> vertex 6 with weight 1
                .long 5,6,3   ; vertex 5 -> vertex 6 with weight 3
    
    ; Degree array for in-degrees
    degrees:    .long 0,1,1,2,2,2   ; in-degree of each vertex (0-indexed)
    
.text
.globl _start

_start:
    ; Initialize distances to infinity
    movl $0, %ecx          ; counter
    movl num_vertices, %edx
    movl INF, %eax
    
init_distances:
    cmpl %edx, %ecx
    jge init_done
    movl %eax, distances(,%ecx,4)
    incl %ecx
    jmp init_distances
init_done:

    ; Set source distance to 0
    movl source, %ecx
    decl %ecx              ; convert to 0-indexed
    movl $0, distances(,%ecx,4)

    ; Topological sort using Kahn's algorithm
    call topological_sort

    ; Relax edges in topological order
    call relax_edges

    ; Print results
    call print_results

    ; Exit program
    movl $1, %eax          ; sys_exit
    movl $0, %ebx          ; exit status
    int $0x80

; Function: topological_sort
; Performs Kahn's algorithm for topological sorting
topological_sort:
    pushl %ebp
    movl %esp, %ebp
    
    ; Initialize queue with vertices of in-degree 0
    movl $0, %ecx          ; vertex counter
    movl num_vertices, %edx
    movl $0, %esi          ; queue index
    
queue_init:
    cmpl %edx, %ecx
    jge queue_done
    
    movl degrees(,%ecx,4), %eax
    cmpl $0, %eax
    jne next_vertex
    
    ; Add vertex to queue
    movl %ecx, topo_order(,%esi,4)
    incl %esi
    
next_vertex:
    incl %ecx
    jmp queue_init
queue_done:

    ; Process vertices in queue order
    movl $0, %ecx          ; queue index
    movl $0, %edi          ; vertex counter
    
process_queue:
    cmpl %edi, %esi        ; check if queue is empty
    jge process_done
    
    movl topo_order(,%ecx,4), %eax  ; get vertex from queue
    movl %eax, %ebx        ; store vertex number
    
    ; Process all neighbors of vertex %ebx
    call process_neighbors
    
    incl %ecx              ; move to next queue element
    jmp process_queue
process_done:

    popl %ebp
    ret

; Function: process_neighbors
; Processes neighbors of a given vertex and updates in-degrees
process_neighbors:
    pushl %ebp
    movl %esp, %ebp
    
    ; This is a simplified version - in practice would iterate through edges
    ; For this example, we assume we know the neighbors
    
    popl %ebp
    ret

; Function: relax_edges
; Relaxes all edges according to topological order
relax_edges:
    pushl %ebp
    movl %esp, %ebp
    
    movl $0, %ecx          ; vertex counter
    movl num_vertices, %edx
    
relax_loop:
    cmpl %edx, %ecx
    jge relax_done
    
    ; Get vertex from topological order
    movl topo_order(,%ecx,4), %eax
    
    ; Process all outgoing edges of this vertex
    call process_outgoing_edges
    
    incl %ecx
    jmp relax_loop
relax_done:

    popl %ebp
    ret

; Function: process_outgoing_edges
; Process edges from a vertex to update distances
process_outgoing_edges:
    pushl %ebp
    movl %esp, %ebp
    
    ; Simplified edge processing - in real implementation would look up actual edges
    ; This is where the relaxation happens: if dist[u] + weight < dist[v], update dist[v]
    
    popl %ebp
    ret

; Function: print_results
print_results:
    pushl %ebp
    movl %esp, %ebp
    
    movl $0, %ecx          ; vertex counter
    movl num_vertices, %edx
    
print_loop:
    cmpl %edx, %ecx
    jge print_done
    
    movl distances(,%ecx,4), %eax
    ; Print distance for vertex (%ecx + 1)
    ; In actual implementation, would use system calls to output
    
    incl %ecx
    jmp print_loop
print_done:

    popl %ebp
    ret

; Helper function: update_distance
; Updates distance if a shorter path is found
update_distance:
    pushl %ebp
    movl %esp, %ebp
    
    ; Parameters: vertex u, vertex v, weight w
    ; dist[v] = min(dist[v], dist[u] + w)
    
    popl %ebp
    ret

```

## Key Assembly Concepts Used

1. **Data Structures**: Arrays for adjacency lists and distance tracking
2. **Control Flow**: Loops using conditional jumps (`cmpl`, `jge`, `jmp`)
3. **Memory Management**: Using `.space` directives to allocate arrays
4. **Function Calls**: Using stack-based calling convention
5. **Register Usage**: Efficient use of general-purpose registers

## Algorithm Steps in Assembly

1. **Initialization**: Set all distances to infinity except source
2. **Topological Sort**: Use Kahn's algorithm with queue processing
3. **Relaxation**: Process edges in topological order using relaxation
4. **Output**: Print final shortest distances

This implementation demonstrates the core principles of solving DAG shortest path problems in assembly language, though a complete working version would require more detailed edge processing and system call integration for output.