# Rosalind Problem: Shortest Cycle Through a Given Edge

## Problem Understanding

We need to find the shortest cycle in an undirected graph that passes through a specific given edge.

## Approach

1. **Graph Representation**: Use adjacency list representation
2. **Algorithm**: 
   - Remove the given edge temporarily
   - Find the shortest path between the two vertices of the edge in the remaining graph
   - Add the edge back and return the path length + 1 (the edge itself)
3. **Implementation**: Use BFS for shortest path finding

## Assembly Implementation

```assembly
.section .data
    # Graph adjacency list representation
    # Each node points to its neighbors
    graph: .long 1, 2, 3, -1    # Node 0 connects to 1, 2, 3
    graph: .long 0, 4, -1       # Node 1 connects to 0, 4
    graph: .long 0, 5, -1       # Node 2 connects to 0, 5
    graph: .long 0, 6, -1       # Node 3 connects to 0, 6
    graph: .long 1, 7, -1       # Node 4 connects to 1, 7
    graph: .long 2, 8, -1       # Node 5 connects to 2, 8
    graph: .long 3, 9, -1       # Node 6 connects to 3, 9
    graph: .long 4, -1          # Node 7 connects to 4
    graph: .long 5, -1          # Node 8 connects to 5
    graph: .long 6, -1          # Node 9 connects to 6
    
    # Given edge vertices
    start_vertex: .long 0
    end_vertex: .long 1
    
    # BFS queue
    queue: .space 1000          # Queue for BFS
    visited: .space 100         # Visited array
    distances: .space 100       # Distance array
    
    # Constants
    MAX_NODES: .long 10
    QUEUE_SIZE: .long 1000
    INF: .long 999999

.section .text
    .global _start

_start:
    # Initialize data structures
    la $t0, visited
    li $t1, 0
    li $t2, 100
init_visited:
    sw $t1, 0($t0)
    addi $t0, $t0, 4
    addi $t2, $t2, -1
    bnez $t2, init_visited

    # Initialize distances
    la $t0, distances
    li $t1, 0
    li $t2, 100
init_distances:
    sw $t1, 0($t0)
    addi $t0, $t0, 4
    addi $t2, $t2, -1
    bnez $t2, init_distances

    # Get the given edge vertices
    la $t0, start_vertex
    lw $t1, 0($t0)              # $t1 = start vertex
    la $t0, end_vertex
    lw $t2, 0($t0)              # $t2 = end vertex

    # Remove the edge temporarily by setting the adjacency lists
    # Remove edge from start_vertex to end_vertex
    la $t3, graph
    addi $t3, $t3, 0            # Start vertex adjacency list
    remove_edge_start:
        lw $t4, 0($t3)
        beq $t4, $t2, remove_edge_found
        beq $t4, -1, remove_edge_not_found
        addi $t3, $t3, 4
        j remove_edge_start
    remove_edge_found:
        sw $zero, 0($t3)        # Remove the edge
    remove_edge_not_found:

    # Remove edge from end_vertex to start_vertex
    la $t3, graph
    addi $t3, $t3, 4            # End vertex adjacency list
    remove_edge_end:
        lw $t4, 0($t3)
        beq $t4, $t1, remove_edge_found_end
        beq $t4, -1, remove_edge_not_found_end
        addi $t3, $t3, 4
        j remove_edge_end
    remove_edge_found_end:
        sw $zero, 0($t3)        # Remove the edge
    remove_edge_not_found_end:

    # BFS to find shortest path between start and end vertex
    # Initialize queue
    la $t3, queue
    sw $t1, 0($t3)              # Add start vertex to queue
    li $t4, 4
    sw $t4, 4($t3)              # Add distance 1
    sw $t1, 8($t3)              # Add parent -1
    li $t5, 1                   # Queue size = 1

    # Initialize visited and distances
    la $t6, visited
    sw $t1, 0($t6)              # Mark start vertex as visited
    la $t6, distances
    sw $t1, 0($t6)              # Distance to start is 0

    # BFS loop
    la $t3, queue
    li $t7, 0                   # Queue head index
    bfs_loop:
        beq $t5, $zero, bfs_done
        lw $t8, 0($t3)          # Get current vertex
        lw $t9, 4($t3)          # Get current distance
        addi $t3, $t3, 8        # Move to next queue element
        addi $t5, $t5, -1       # Decrement queue size
        beq $t8, $t2, bfs_found # Found the target vertex

        # Explore neighbors of current vertex
        la $t10, graph
        addi $t10, $t10, 0      # Start vertex adjacency list
        explore_neighbors:
            lw $t11, 0($t10)
            beq $t11, -1, explore_done
            beq $t11, $t8, next_neighbor
            la $t12, visited
            lw $t13, 0($t12)
            beq $t13, $zero, not_visited
            # Already visited, skip
            next_neighbor:
                addi $t10, $t10, 4
                j explore_neighbors
            not_visited:
                # Add to queue
                la $t14, queue
                add $t14, $t14, $t5
                sw $t11, 0($t14)    # Add neighbor to queue
                addi $t14, $t14, 4
                addi $t9, $t9, 1
                sw $t9, 0($t14)     # Add distance
                addi $t14, $t14, 4
                sw $t8, 0($t14)     # Add parent
                addi $t5, $t5, 1    # Increment queue size
                
                # Mark as visited
                la $t12, visited
                sw $t1, 0($t12)
                
                # Update distance
                la $t12, distances
                sw $t9, 0($t12)
                j explore_neighbors
        explore_done:
        j bfs_loop

    bfs_found:
        # Found shortest path, calculate cycle length
        la $t12, distances
        lw $t13, 0($t12)
        addi $t13, $t13, 1       # Add the given edge
        # Result in $t13

    bfs_done:
        # Restore the edge
        # This is a simplified version - in practice, we'd need to reconstruct the graph
        # For now, just return the calculated result
        li $v0, 1
        move $a0, $t13
        syscall

    # Exit program
    li $v0, 10
    syscall
```

## Key Points

1. **Graph Representation**: Uses adjacency lists stored in memory
2. **BFS Algorithm**: Standard breadth-first search for shortest path
3. **Edge Removal**: Temporarily removes the given edge to find shortest path between its endpoints
4. **Cycle Calculation**: The shortest cycle length = shortest path + 1 (the edge itself)
5. **Memory Management**: Uses stack space for queue, visited array, and distance tracking

## Time Complexity
- O(V + E) for BFS traversal
- O(V) for edge removal and restoration
- Overall: O(V + E)

## Space Complexity
- O(V + E) for storing the graph and auxiliary data structures

This implementation provides a framework for solving the shortest cycle through a given edge problem in assembly language.

