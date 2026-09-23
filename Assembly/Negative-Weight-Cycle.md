# Rosalind Problem: Negative Weight Cycle (Assembly Solution)

## Problem Understanding

The negative weight cycle problem asks us to detect if there's a negative cycle in a weighted directed graph. A negative cycle is a cycle where the sum of edge weights is negative, which can cause issues in shortest path algorithms.

## Approach

I'll implement the Bellman-Ford algorithm with modifications to detect negative cycles:

1. Run Bellman-Ford for n-1 iterations
2. Perform one additional iteration to check for relaxations
3. If any relaxation occurs in the final iteration, a negative cycle exists

## Assembly Implementation

```assembly
.section .data
    # Graph representation - adjacency list
    # For simplicity, we'll assume a fixed-size graph
    MAX_NODES: .long 1000
    MAX_EDGES: .long 2000
    
    # Graph storage
    edges: .space 2000 * 8    # Each edge: (from, to, weight)
    dist: .space 1000 * 4     # Distance array
    parent: .space 1000 * 4   # Parent array for path reconstruction
    
    # Input parameters
    n_nodes: .long 0          # Number of nodes
    n_edges: .long 0          # Number of edges
    
.section .text
.globl _start

_start:
    # Read input data (simplified version)
    # In practice, this would read from stdin
    movl $10, n_nodes         # Example: 10 nodes
    movl $15, n_edges         # Example: 15 edges
    
    # Initialize distances to infinity (0x7FFFFFFF)
    call initialize_distances
    
    # Set source distance to 0
    movl $0, dist(,%eax,4)    # dist[0] = 0
    
    # Main Bellman-Ford algorithm
    movl n_nodes, %ecx        # ecx = number of nodes
    decl %ecx                 # Loop for n-1 iterations
    
bellman_ford_loop:
    # For each edge, relax it
    call relax_all_edges
    
    # Decrement iteration counter
    decl %ecx
    jnz bellman_ford_loop
    
    # Final check for negative cycle
    call check_negative_cycle
    
    # Exit program
    movl $1, %eax             # sys_exit
    movl $0, %ebx             # exit status
    int $0x80

# Initialize distances array to infinity
initialize_distances:
    pushl %eax
    pushl %ecx
    
    xorl %ecx, %ecx           # i = 0
init_loop:
    cmpl n_nodes, %ecx        # while i < n_nodes
    jge init_end
    
    movl $0x7FFFFFFF, dist(,%ecx,4)  # dist[i] = INF
    incl %ecx
    jmp init_loop
    
init_end:
    popl %ecx
    popl %eax
    ret

# Relax all edges for current iteration
relax_all_edges:
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $0, %ecx             # edge_index = 0
relax_edge_loop:
    cmpl n_edges, %ecx        # while edge_index < n_edges
    jge relax_end
    
    # Load edge data
    movl edges(,%ecx,8), %eax      # from
    movl edges+4(,%ecx,8), %ebx    # to
    movl edges+8(,%ecx,8), %edx    # weight
    
    # Check if relaxation possible
    movl dist(,%eax,4), %esi      # dist[from]
    cmpl $0x7FFFFFFF, %esi        # if dist[from] != INF
    je relax_continue
    
    movl dist(,%eax,4), %esi
    addl %edx, %esi               # dist[from] + weight
    
    cmpl dist(,%ebx,4), %esi     # if dist[from] + weight < dist[to]
    jge relax_continue
    
    # Relax edge: dist[to] = dist[from] + weight
    movl %esi, dist(,%ebx,4)
    
relax_continue:
    incl %ecx
    jmp relax_edge_loop
    
relax_end:
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

# Check for negative cycle after n-1 iterations
check_negative_cycle:
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $0, %ecx             # edge_index = 0
check_loop:
    cmpl n_edges, %ecx        # while edge_index < n_edges
    jge check_end
    
    # Load edge data
    movl edges(,%ecx,8), %eax      # from
    movl edges+4(,%ecx,8), %ebx    # to
    movl edges+8(,%ecx,8), %edx    # weight
    
    # Check if relaxation possible
    movl dist(,%eax,4), %esi      # dist[from]
    cmpl $0x7FFFFFFF, %esi        # if dist[from] != INF
    je check_continue
    
    movl dist(,%eax,4), %esi
    addl %edx, %esi               # dist[from] + weight
    
    cmpl dist(,%ebx,4), %esi     # if dist[from] + weight < dist[to]
    jge check_continue
    
    # Negative cycle detected!
    # Set flag or print result
    movl $1, %eax                 # Indicate negative cycle found
    
check_continue:
    incl %ecx
    jmp check_loop
    
check_end:
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

# Helper function to print result
print_result:
    pushl %eax
    pushl %ebx
    
    # Print "1" if negative cycle found, "0" otherwise
    movl $1, %eax                 # sys_write
    movl $1, %ebx                 # stdout
    movl $result_msg, %ecx        # message
    movl $1, %edx                 # length
    
    int $0x80
    
    popl %ebx
    popl %eax
    ret

.section .data
    result_msg: .ascii "1\n"     # Placeholder for actual output
```

## Key Algorithm Steps

1. **Initialization**: Set all distances to infinity except source (0)
2. **Relaxation**: For n-1 iterations, relax all edges
3. **Negative Cycle Detection**: In final iteration, check if any edge can still be relaxed
4. **Output**: Return 1 if negative cycle found, 0 otherwise

## Time Complexity
- O(V×E) where V is vertices and E is edges
- Space complexity: O(V + E)

## Notes for Rosalind Implementation

For the actual Rosalind submission:
- Read input from stdin in proper format
- Handle multiple test cases if needed
- Output exactly "1" or "0" as required by the problem
- Consider using more efficient I/O methods for large inputs

This assembly implementation provides the core logic for negative weight cycle detection using Bellman-Ford algorithm adapted for assembly programming.