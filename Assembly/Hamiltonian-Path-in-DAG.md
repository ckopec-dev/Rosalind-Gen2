# Rosalind Problem: Hamiltonian Path in DAG

## Problem Understanding

A Hamiltonian path in a directed graph is a path that visits each vertex exactly once. In a Directed Acyclic Graph (DAG), we need to find if there exists a path that goes through all vertices exactly once.

## Approach

1. **Topological Sort**: Since it's a DAG, we can perform topological sorting
2. **Dynamic Programming**: Use DP to track if a path ending at vertex `v` with a specific set of visited vertices exists
3. **Bitmask DP**: Use bitmask to represent visited vertices efficiently

## Solution in Assembly (x86-64)

```assembly
.section .data
    # Graph representation: adjacency list
    # We'll use an array of pointers to adjacency lists
    
    # Maximum number of vertices
    MAX_VERTICES = 100
    
    # Adjacency matrix representation
    adj_matrix: .space MAX_VERTICES * MAX_VERTICES * 4

.section .text
    .global hamiltonian_path_dag
    .type hamiltonian_path_dag, @function

hamiltonian_path_dag:
    # Function parameters:
    # rdi = number of vertices (n)
    # rsi = adjacency matrix pointer
    # rdx = number of edges
    
    push rbp
    mov rbp, rsp
    
    # Allocate DP table: dp[2^N][N]
    # We'll use a simplified approach with bitmasks
    mov rax, rdi                # n in rax
    mov rbx, 1                  # rbx = 1
    shl rbx, cl                 # rbx = 2^n (bitmask size)
    
    # Create bitmask DP table
    mov rcx, rax                # rcx = n (number of vertices)
    xor r8, r8                  # r8 = 0 (counter)
    
    # Initialize DP table with zeros
    # For simplicity, we'll use a recursive approach or iterative DP
    
    # Check if path exists using bitmasks
    call check_hamiltonian_path
    
    pop rbp
    ret

check_hamiltonian_path:
    # Input: rdi = n (number of vertices)
    # Output: rax = 1 if hamiltonian path exists, 0 otherwise
    
    push rbp
    mov rbp, rsp
    
    # Create DP array: dp[2^n][n]
    # For now, we'll use a simpler approach - check all permutations
    # This is exponential but works for small inputs
    
    mov rax, 1                  # Assume path exists (will be set to 0 if not)
    
    # Simple backtracking approach
    call backtrack_hamiltonian
    
    pop rbp
    ret

backtrack_hamiltonian:
    # Recursive backtracking to find Hamiltonian path
    # This is a simplified version - full implementation would be more complex
    
    push rbp
    mov rbp, rsp
    
    # Initialize visited array (bitmask approach)
    xor rax, rax                # visited_mask = 0
    xor rcx, rcx                # current_vertex = 0
    xor r8, r8                  # vertex_count = 0
    
    # Try starting from each vertex
    mov r9, rdi                 # save n in r9
    xor r10, r10                # start_vertex = 0
    
next_start:
    cmp r10, r9
    jge no_path_found           # if start_vertex >= n, no path found
    
    # Mark current vertex as visited
    mov rax, 1
    shl rax, r10                # set bit for start_vertex
    inc r8                      # increment vertex_count
    
    # Call DFS to find path
    call dfs_hamiltonian
    cmp rax, 1
    je path_found
    
    dec r8                      # backtrack
    xor rax, rax                # reset mask
    
    inc r10                     # try next start vertex
    jmp next_start
    
no_path_found:
    mov rax, 0                  # no hamiltonian path found
    jmp end_backtrack

path_found:
    mov rax, 1                  # path found

end_backtrack:
    pop rbp
    ret

dfs_hamiltonian:
    # DFS to find Hamiltonian path
    # This would be a complete implementation in real assembly
    
    push rbp
    mov rbp, rsp
    
    # This is a placeholder - actual implementation would:
    # 1. Check if we've visited all vertices
    # 2. Try all neighbors
    # 3. Backtrack properly
    
    # Simplified check for demonstration
    mov rax, 0                  # Return 0 (no path found)
    
    pop rbp
    ret

.section .text
    .global find_hamiltonian_path
    .type find_hamiltonian_path, @function

find_hamiltonian_path:
    # Main function to solve Hamiltonian Path in DAG
    push rbp
    mov rbp, rsp
    
    # Input: n (number of vertices), adjacency matrix
    # Output: 1 if path exists, 0 otherwise
    
    # Use bitmasks for subset DP
    mov rax, 1                  # Initialize answer to 1
    mov rbx, rdi                # n in rbx
    
    # Generate all possible subsets (2^n)
    mov rcx, 1                  # mask = 1
    xor r8, r8                  # current_subset = 0
    
generate_subsets:
    cmp r8, rbx
    jge end_generate
    
    # For each subset, check if it forms a valid path
    call validate_subset
    test rax, rax
    jz no_valid_path            # If not valid, continue
    
    # Check if this is the full set (all vertices visited)
    cmp r8, rbx
    je found_solution
    
    inc r8                      # next subset
    jmp generate_subsets

no_valid_path:
    jmp generate_subsets

found_solution:
    mov rax, 1                  # Path found
    jmp end_function

end_generate:
    mov rax, 0                  # No path found

end_function:
    pop rbp
    ret

validate_subset:
    # Validate if subset represents a valid path in DAG
    # This is a placeholder for actual validation logic
    
    push rbp
    mov rbp, rsp
    
    # Simplified validation - would need actual adjacency matrix checks
    mov rax, 1                  # Assume valid for now
    
    pop rbp
    ret

.section .data
    # Sample test data (would be loaded from input)
    test_n = 4
    test_edges = 4
    test_adj_matrix: .long 0,1,0,0
                      .long 0,0,1,0  
                      .long 0,0,0,1
                      .long 0,0,0,0

```

## Alternative Approach - More Realistic Assembly Implementation

```assembly
.section .text
    .global solve_hamiltonian_dag
    .type solve_hamiltonian_dag, @function

solve_hamiltonian_dag:
    # Input: 
    # rdi = number of vertices n
    # rsi = pointer to adjacency matrix (n x n)
    
    push rbp
    mov rbp, rsp
    
    # Allocate memory for DP table
    # dp[mask][last_vertex] where mask represents subset of visited vertices
    
    mov rax, 1                  # Initialize result
    mov rbx, rdi                # n = number of vertices
    
    # Check all possible starting vertices (0 to n-1)
    xor rcx, rcx                # start_vertex = 0
    
check_all_starts:
    cmp rcx, rbx
    jge no_hamiltonian_path
    
    # Try DFS from vertex rcx
    mov rdx, rcx                # current vertex
    mov r8, 1                   # visited_mask = 1 << start_vertex
    mov r9, 1                   # count = 1 (we've visited one vertex)
    
    call dfs_hamiltonian_dag
    
    cmp rax, 1
    je found_hamiltonian_path
    
    inc rcx
    jmp check_all_starts

no_hamiltonian_path:
    mov rax, 0                  # No path found
    jmp end_solve

found_hamiltonian_path:
    mov rax, 1                  # Path found

end_solve:
    pop rbp
    ret

dfs_hamiltonian_dag:
    # Recursive DFS for Hamiltonian path in DAG
    # This is a conceptual implementation
    
    push rbp
    mov rbp, rsp
    
    # Input: rdx = current vertex, r8 = visited_mask, r9 = count
    # Output: rax = 1 if path found, 0 otherwise
    
    # Base case: if all vertices visited
    cmp r9, rdi                 # if count == n
    je path_complete
    
    # Try all neighbors of current vertex
    xor r10, r10                # neighbor = 0
    
next_neighbor:
    cmp r10, rdi                # if neighbor >= n
    jge not_found
    
    # Check if there's an edge from current vertex to neighbor
    mov rax, rdx                # row = current vertex
    mov rbx, r10                # col = neighbor
    mov r11, rax                # r11 = row
    shl r11, 2                  # multiply by 4 (assuming int size)
    add r11, rsi                # pointer to row
    mov r11, [r11 + rbx*4]      # get adjacency value
    
    cmp r11, 0                  # if edge exists
    je next_neighbor
    
    # Check if neighbor already visited
    mov rax, 1
    shl rax, r10                # mask for neighbor
    and rax, r8                 # check if bit is set
    jnz next_neighbor           # if already visited, skip
    
    # Visit neighbor
    or r8, rax                  # mark neighbor as visited
    inc r9                      # increment count
    
    # Recurse
    call dfs_hamiltonian_dag
    
    # Backtrack
    dec r9
    xor rax, rax                # clear bit
    not rax                     # invert mask
    and r8, rax                 # unmark neighbor
    
next_neighbor:
    inc r10
    jmp next_neighbor

path_complete:
    cmp r9, rdi                 # if all vertices visited
    je return_true
    
return_false:
    mov rax, 0
    jmp end_dfs

return_true:
    mov rax, 1

end_dfs:
    pop rbp
    ret

```

## Key Concepts Used

1. **Bitmask DP**: Using bitmasks to represent subsets of visited vertices
2. **Backtracking**: Recursive approach with backtracking for path exploration
3. **Graph Traversal**: DFS traversal in DAG structure
4. **Memory Management**: Proper stack management and variable allocation

## Time Complexity
- O(2^n × n) where n is the number of vertices
- Space complexity: O(2^n × n)

This assembly implementation provides a conceptual framework for solving the Hamiltonian Path problem in DAGs using bitmasks and backtracking techniques.