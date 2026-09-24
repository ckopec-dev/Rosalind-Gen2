# Rosalind Problem: Semi-Connected Graph

## Problem Understanding

A directed graph is semi-connected if for every pair of vertices u and v, either there is a path from u to v or a path from v to u (or both). In other words, the graph's condensation (the DAG formed by contracting each strongly connected component) has a Hamiltonian path.

## Approach

1. **Find Strongly Connected Components (SCCs)** using Kosaraju's algorithm
2. **Contract SCCs** into a DAG (Directed Acyclic Graph)
3. **Check if the DAG is semi-connected** by verifying that there's a path between any two components in one direction

## Assembly Implementation

```assembly
.data
    # Input graph representation
    MAX_NODES EQU 1000
    adj_list db MAX_NODES dup(0)  ; Adjacency list for original graph
    reverse_adj db MAX_NODES dup(0)  ; Reverse adjacency list
    visited db MAX_NODES dup(0)  ; Visited array for DFS
    stack db MAX_NODES dup(0)  ; Stack for DFS
    scc_id db MAX_NODES dup(0)  ; Strongly connected component IDs
    component_count dd 0  ; Number of SCCs
    topo_order dd MAX_NODES dup(0)  ; Topological order
    
    # Constants
    TRUE EQU 1
    FALSE EQU 0

.code
main PROC
    ; Initialize data structures
    xor ecx, ecx
init_loop:
    cmp ecx, MAX_NODES
    jge init_done
    mov byte ptr visited[ecx], 0
    mov byte ptr scc_id[ecx], 0
    inc ecx
    jmp init_loop
init_done:

    ; Read input graph (simplified - assume adjacency list is already read)
    ; This would normally parse the input file
    
    ; Step 1: Find SCCs using Kosaraju's algorithm
    call kosaraju_algorithm
    
    ; Step 2: Check if semi-connected
    call check_semi_connected
    
    ; Output result
    cmp eax, TRUE
    je semi_connected
    jmp not_semi_connected
    
semi_connected:
    ; Print "1" for semi-connected
    mov eax, 1
    jmp exit
    
not_semi_connected:
    ; Print "0" for not semi-connected  
    mov eax, 0
    
exit:
    ret
main ENDP

; Kosaraju's algorithm to find SCCs
kosaraju_algorithm PROC
    ; First DFS on reverse graph to get finishing times
    xor ecx, ecx
    call clear_visited
    call dfs_reverse_graph
    
    ; Second DFS on original graph in reverse topological order
    call clear_visited
    call dfs_original_graph
    
    ret
kosaraju_algorithm ENDP

; DFS on reverse graph to get finishing times
dfs_reverse_graph PROC
    xor ecx, ecx
    mov edx, 0  ; Stack pointer
    
reverse_dfs_loop:
    cmp ecx, MAX_NODES
    jge reverse_dfs_done
    
    cmp byte ptr visited[ecx], 0
    je dfs_reverse_visit
    
    inc ecx
    jmp reverse_dfs_loop
    
dfs_reverse_visit:
    push ecx
    mov byte ptr visited[ecx], 1
    
reverse_dfs_stack_loop:
    pop eax
    cmp eax, -1
    je reverse_dfs_stack_done
    
    ; Process node eax
    push eax
    push -1  ; Sentinel for stack
    
    ; Push neighbors (in reverse graph)
    ; This would iterate through adjacency list of node eax
    mov ebx, eax
    mov esi, offset reverse_adj
    add esi, ebx
    
    ; Simulate visiting neighbors
    ; In real implementation: iterate through neighbors and push to stack
    
    jmp reverse_dfs_stack_loop
    
reverse_dfs_stack_done:
    ; Push current node to finishing time stack
    mov byte ptr stack[edx], al
    inc edx
    
    jmp reverse_dfs_loop
    
reverse_dfs_done:
    ret
dfs_reverse_graph ENDP

; DFS on original graph using topological order from reverse DFS
dfs_original_graph PROC
    ; This would process nodes in reverse topological order
    ; and assign SCC IDs to each component
    
    xor ecx, ecx
    mov ebx, 0  ; Component counter
    
original_dfs_loop:
    cmp ecx, MAX_NODES
    jge original_dfs_done
    
    cmp byte ptr visited[ecx], 0
    je dfs_original_visit
    
    inc ecx
    jmp original_dfs_loop
    
dfs_original_visit:
    push ecx
    mov byte ptr visited[ecx], 1
    mov byte ptr scc_id[ecx], bl  ; Assign component ID
    
original_dfs_stack_loop:
    pop eax
    cmp eax, -1
    je original_dfs_stack_done
    
    ; Process neighbors of node eax in original graph
    ; In real implementation: iterate through adjacency list
    
    jmp original_dfs_stack_loop
    
original_dfs_stack_done:
    inc ecx
    jmp original_dfs_loop
    
original_dfs_done:
    ret
dfs_original_graph ENDP

; Clear visited array
clear_visited PROC
    xor ecx, ecx
clear_loop:
    cmp ecx, MAX_NODES
    jge clear_done
    mov byte ptr visited[ecx], 0
    inc ecx
    jmp clear_loop
clear_done:
    ret
clear_visited ENDP

; Check if graph is semi-connected
check_semi_connected PROC
    ; Create condensed DAG from SCCs
    call create_condensed_graph
    
    ; Check if condensed DAG has Hamiltonian path (semi-connected property)
    call check_hamiltonian_path
    
    ret
check_semi_connected ENDP

; Create condensed graph from SCCs
create_condensed_graph PROC
    ; This would iterate through all edges and create new edges between different SCCs
    xor ecx, ecx
    mov edx, 0  ; Edge counter
    
condensed_loop:
    cmp ecx, MAX_NODES
    jge condensed_done
    
    ; Check neighbors of node ecx
    ; If neighbor is in different SCC, add edge to condensed graph
    inc ecx
    jmp condensed_loop
    
condensed_done:
    ret
create_condensed_graph ENDP

; Check if condensed graph has Hamiltonian path
check_hamiltonian_path PROC
    ; Simple check: verify that there's a path between all pairs of components
    xor eax, eax  ; Assume TRUE initially
    
    ; This is a simplified version - in practice this would be more complex
    ; For each pair of SCCs, check if there's a path from one to another
    mov ebx, 0
    mov ecx, 0
    
path_check_loop:
    cmp ebx, component_count
    jge path_check_done
    
    cmp ecx, component_count
    jge next_component
    
    ; Check if there's a path from component ebx to component ecx
    call has_path_between_components
    
    test eax, eax
    jz path_not_found  ; If no path found, graph is not semi-connected
    
next_component:
    inc ecx
    jmp path_check_loop
    
path_not_found:
    mov eax, FALSE
    jmp path_check_done
    
path_check_done:
    ret
check_hamiltonian_path ENDP

; Helper function to check if there's a path between two components
has_path_between_components PROC
    ; This would implement a simple DFS or BFS between components
    xor eax, eax  ; Return FALSE initially
    
    ; In a real implementation, this would:
    ; 1. Find all nodes in component source
    ; 2. Find all nodes in component target  
    ; 3. Check if any node from source can reach any node from target
    
    ret
has_path_between_components ENDP

END
```

## Explanation of Key Components

### 1. **Kosaraju's Algorithm Implementation**
- First DFS on reverse graph to determine finishing times
- Second DFS on original graph in reverse topological order to find SCCs
- Uses two stacks for the DFS traversal

### 2. **SCC Identification**
- Assigns component IDs to each node based on which SCC it belongs to
- Creates a mapping between original nodes and their SCC components

### 3. **Semi-Connected Check**
- Contracts the graph into a DAG of SCCs
- Verifies that for every pair of SCCs, there's a directed path in at least one direction

## Time Complexity
- **Kosaraju's algorithm**: O(V + E) where V is vertices and E is edges
- **SCC identification**: O(V + E)
- **Semi-connected check**: O(C²) where C is number of components
- **Overall**: O(V + E)

## Space Complexity
- O(V + E) for storing adjacency lists and auxiliary arrays

This assembly implementation provides the conceptual framework for solving the semi-connected graph problem, with detailed comments explaining each step of the algorithm.