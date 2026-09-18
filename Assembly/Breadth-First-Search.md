# Rosalind Problem: Breadth-First Search (BFS) - Assembly Solution

## Problem Understanding

The BFS problem asks us to find the shortest path distances from a source node to all other nodes in an unweighted graph using breadth-first search.

## Assembly Implementation

Here's a solution using x86-64 assembly with Linux system calls:

```assembly
.section .data
    # Graph representation as adjacency list
    # Format: [node][neighbor1][neighbor2][...][0]
    
    # Example graph: 5 nodes, edges: (0,1), (0,2), (1,3), (1,4)
    graph:      .long 0, 1, 2, 0
                .long 1, 3, 4, 0
                .long 2, 0
                .long 3, 1, 0
                .long 4, 1, 0
    
    # BFS queue for node processing
    queue:      .long 0, 0, 0, 0, 0  # Queue of size 5
    visited:    .byte 0, 0, 0, 0, 0   # Visited array (5 nodes)
    distances:  .long 0, 0, 0, 0, 0   # Distance array
    
    source:     .long 0               # Starting node
    
    # Constants
    MAX_NODES:  .long 5
    QUEUE_SIZE: .long 5

.section .text
    .global _start

# Function to perform BFS
bfs:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters: %rdi = source node
    # Local variables:
    # %rax = current node
    # %rbx = queue index
    # %rcx = neighbor index
    # %rdx = distance
    
    # Initialize visited array to 0
    mov $visited, %rdi
    mov $5, %rcx
    xor %al, %al
    rep stosb
    
    # Initialize distances array to -1 (unreachable)
    mov $distances, %rdi
    mov $5, %rcx
    mov $-1, %eax
    rep stosb
    
    # Set source distance to 0
    mov $source, %rax
    movl (%rax), %ebx
    movl $0, distances(%rbx, 4)
    
    # Initialize queue with source node
    mov $queue, %rdi
    movl $0, (%rdi)  # Queue head = 0
    movl $1, 4(%rdi) # Queue tail = 1
    movl $source, 4(%rdi)  # Add source to queue
    
    # Mark source as visited
    mov $visited, %rdi
    movl $0, (%rdi)  # visited[0] = 1
    
    # BFS loop
bfs_loop:
    # Check if queue is empty
    mov $queue, %rax
    movl 0(%rax), %ebx  # head
    movl 4(%rax), %ecx  # tail
    
    cmp %ecx, %ebx
    je bfs_done  # If head == tail, queue is empty
    
    # Remove node from queue
    mov $queue, %rax
    movl 0(%rax), %ebx  # head
    addl $1, 0(%rax)    # head++
    
    # Get current node
    movl (%rax, %rbx, 4), %eax
    
    # Process neighbors of current node
    call get_neighbors
    mov %rax, %rcx  # neighbor list pointer
    
neighbor_loop:
    # Check if neighbor exists (not 0)
    movl (%rcx), %eax
    test %eax, %eax
    jz neighbor_done
    
    # If not visited, process it
    mov $visited, %rdi
    movl (%rcx), %ebx
    testb %al, (%rdi, %rbx)  # Check if already visited
    jnz neighbor_skip
    
    # Mark as visited
    mov $visited, %rdi
    movl (%rcx), %ebx
    movb $1, (%rdi, %rbx)
    
    # Add to queue
    mov $queue, %rdi
    movl 4(%rdi), %ebx  # tail
    addl $1, 4(%rdi)    # tail++
    movl (%rcx), (%rdi, %rbx, 4)
    
    # Update distance
    mov $distances, %rdi
    movl (%rcx), %ebx
    movl (%rdi, %rbx, 4), %edx  # Get current distance
    addl $1, %edx               # Distance + 1
    movl %edx, (%rdi, %rbx, 4)
    
neighbor_skip:
    addl $4, %rcx  # Next neighbor
    jmp neighbor_loop
    
neighbor_done:
    jmp bfs_loop

bfs_done:
    pop %rbp
    ret

# Function to get neighbors of a node
get_neighbors:
    push %rbp
    mov %rsp, %rbp
    
    # %rdi = node index
    # Return: pointer to neighbor list
    
    # Calculate offset in graph array
    mov $graph, %rax
    # Assuming each node's neighbor list starts at a fixed offset
    # This is a simplified implementation - actual graph structure would need to be parsed
    
    pop %rbp
    ret

# Main program
_start:
    # Initialize data structures
    call bfs
    
    # Print results (simplified)
    mov $1, %rax        # sys_exit
    mov $0, %rdi        # exit status
    syscall
```

## Key Components Explained

### Data Structures
- **Graph representation**: Adjacency list stored as arrays
- **Queue**: Simple array-based queue for BFS traversal
- **Visited array**: Tracks which nodes have been processed
- **Distances array**: Stores shortest distances from source

### Algorithm Steps
1. Initialize all data structures
2. Mark source node as visited and distance 0
3. Add source to queue
4. While queue not empty:
   - Remove node from queue
   - Process each neighbor of current node
   - If neighbor unvisited, mark as visited, add to queue, update distance

### System Calls
The code uses Linux system calls for:
- Memory management (initialization)
- Program termination

## Usage Notes

This assembly implementation provides a conceptual framework for BFS. In practice, you would need:

1. Proper graph parsing from input
2. Complete neighbor list traversal logic
3. Output formatting for results
4. Error handling for edge cases

The actual Rosalind problem requires parsing the input graph and returning distance values in a specific format, which would require additional assembly code to handle file I/O and output formatting.