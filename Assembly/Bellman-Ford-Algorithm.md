# Bellman-Ford Algorithm Implementation in Assembly

I'll implement the Bellman-Ford algorithm to find shortest paths from a source vertex to all other vertices in a weighted directed graph.

## Problem Analysis

The Bellman-Ford algorithm:
1. Initialize distances from source to all vertices as infinity, except source = 0
2. Relax all edges V-1 times
3. Check for negative cycles

## Assembly Implementation

```assembly
.section .data
    # Graph representation: edges in format (from, to, weight)
    edges: .long 0, 1, 4      # Edge from 0 to 1 with weight 4
           .long 0, 2, 2      # Edge from 0 to 2 with weight 2
           .long 1, 2, -3     # Edge from 1 to 2 with weight -3
           .long 1, 3, 2      # Edge from 1 to 3 with weight 2
           .long 2, 3, 3      # Edge from 2 to 3 with weight 3
           .long 2, 4, 2      # Edge from 2 to 4 with weight 2
           .long 3, 4, -5     # Edge from 3 to 4 with weight -5
    
    num_edges: .long 7        # Number of edges
    num_vertices: .long 5     # Number of vertices
    source_vertex: .long 0    # Source vertex
    
    # Distance array initialization
    distances: .long 0, 2147483647, 2147483647, 2147483647, 2147483647
    # Initialize with infinity (2^31 - 1) except source = 0
    
    # Temporary distance array for relaxation
    temp_distances: .space 20  # 5 vertices * 4 bytes each

.section .text
    .global _start

_start:
    # Load parameters
    la $t0, edges           # Load address of edges array
    lw $t1, num_edges       # Load number of edges
    lw $t2, num_vertices    # Load number of vertices
    lw $t3, source_vertex   # Load source vertex
    
    # Initialize distance array with infinity
    la $t4, distances       # Load address of distances array
    li $t5, 0               # Counter for vertices
    li $t6, 2147483647      # Infinity value
    
init_loop:
    beq $t5, $t2, init_done  # If counter == num_vertices, done
    sw $t6, 0($t4)          # Set current distance to infinity
    addi $t4, $t4, 4        # Move to next vertex
    addi $t5, $t5, 1        # Increment counter
    j init_loop
    
init_done:
    # Set source distance to 0
    li $t5, 0
    la $t4, distances
    sw $zero, 0($t4)        # Source vertex distance = 0
    
    # Bellman-Ford algorithm main loop - relax edges V-1 times
    addi $t7, $t2, -1       # V-1 iterations
    li $t8, 0               # Iteration counter
    
bellman_ford_loop:
    beq $t8, $t7, check_negative_cycle  # If we've done V-1 iterations, check for negative cycles
    
    # Relax all edges
    la $t4, edges           # Reset edge pointer
    li $t9, 0               # Edge counter
    
relax_edge_loop:
    beq $t9, $t1, next_iteration  # If we've processed all edges
    
    # Load edge data: from, to, weight
    lw $t0, 0($t4)          # From vertex
    lw $t1, 4($t4)          # To vertex
    lw $t2, 8($t4)          # Weight
    
    # Load current distance of 'from' vertex
    la $t3, distances
    sll $t0, $t0, 2         # Multiply by 4 (word size)
    add $t3, $t3, $t0       # Address of from vertex distance
    lw $t5, 0($t3)          # Current distance to 'from' vertex
    
    # Check if we can improve the distance to 'to' vertex
    beq $t5, 2147483647, next_edge  # If from distance is infinity, skip
    
    # Load current distance to 'to' vertex
    la $t3, distances
    sll $t1, $t1, 2         # Multiply by 4 (word size)
    add $t3, $t3, $t1       # Address of to vertex distance
    lw $t6, 0($t3)          # Current distance to 'to' vertex
    
    # Calculate new potential distance
    add $t1, $t5, $t2       # New distance = from_distance + weight
    
    # If new distance is smaller, update
    bge $t1, $t6, next_edge  # If new >= current, no improvement
    
    # Update distance
    sw $t1, 0($t3)          # Store new distance to 'to' vertex
    
next_edge:
    addi $t4, $t4, 12       # Move to next edge (3 words * 4 bytes)
    addi $t9, $t9, 1        # Increment edge counter
    j relax_edge_loop
    
next_iteration:
    addi $t8, $t8, 1        # Increment iteration counter
    j bellman_ford_loop
    
check_negative_cycle:
    # Check for negative cycles by relaxing once more
    la $t4, edges           # Reset edge pointer
    li $t9, 0               # Edge counter
    
negative_cycle_check:
    beq $t9, $t1, print_results  # If all edges processed, print results
    
    # Load edge data: from, to, weight
    lw $t0, 0($t4)          # From vertex
    lw $t1, 4($t4)          # To vertex
    lw $t2, 8($t4)          # Weight
    
    # Load current distance of 'from' vertex
    la $t3, distances
    sll $t0, $t0, 2         # Multiply by 4 (word size)
    add $t3, $t3, $t0       # Address of from vertex distance
    lw $t5, 0($t3)          # Current distance to 'from' vertex
    
    # Check if we can improve the distance to 'to' vertex
    beq $t5, 2147483647, next_edge_check  # If from distance is infinity, skip
    
    # Load current distance to 'to' vertex
    la $t3, distances
    sll $t1, $t1, 2         # Multiply by 4 (word size)
    add $t3, $t3, $t1       # Address of to vertex distance
    lw $t6, 0($t3)          # Current distance to 'to' vertex
    
    # Calculate new potential distance
    add $t1, $t5, $t2       # New distance = from_distance + weight
    
    # If we can still improve, there's a negative cycle
    bge $t1, $t6, next_edge_check  # If new >= current, no improvement
    
    # Negative cycle detected - terminate with error
    li $v0, 1               # Exit system call
    li $a0, 1               # Error code
    syscall
    
next_edge_check:
    addi $t4, $t4, 12       # Move to next edge (3 words * 4 bytes)
    addi $t9, $t9, 1        # Increment edge counter
    j negative_cycle_check
    
print_results:
    # Print final distances
    la $t4, distances
    li $t5, 0               # Vertex counter
    
print_loop:
    beq $t5, $t2, exit      # If all vertices printed, exit
    
    lw $a0, 0($t4)          # Load distance
    li $v0, 1               # Print integer system call
    syscall
    
    # Print newline (ASCII 10)
    li $v0, 11              # Print character system call
    li $a0, 10              # Newline character
    syscall
    
    addi $t4, $t4, 4        # Move to next vertex
    addi $t5, $t5, 1        # Increment counter
    j print_loop

exit:
    # Exit program
    li $v0, 1               # Exit system call
    li $a0, 0               # Normal exit code
    syscall
```

## Algorithm Explanation

### Key Steps:

1. **Initialization**: Set all distances to infinity except source vertex (0)
2. **Relaxation**: For V-1 iterations, relax all edges:
   - If `distance[u] + weight(u,v) < distance[v]`, update `distance[v]`
3. **Negative Cycle Detection**: Check if any edge can still be relaxed
4. **Output**: Print final shortest distances from source

### Time Complexity: O(VE)
### Space Complexity: O(V)

## Key Assembly Concepts Used:

- **Memory addressing** for graph representation
- **Loop control** with counters and conditional branches
- **Arithmetic operations** for distance calculations
- **System calls** for input/output operations
- **Pointer manipulation** to traverse edges and vertices

This implementation assumes a specific system call interface (like SPIM or MARS) for I/O operations and handles the standard Bellman-Ford algorithm with negative cycle detection.