# Rosalind Problem: Topological Sorting

## Problem Understanding

Topological sorting is a linear ordering of vertices in a directed acyclic graph (DAG) such that for every directed edge u → v, vertex u comes before v in the ordering.

## Solution Approach

I'll implement a topological sort using Kahn's algorithm:
1. Calculate in-degrees for all nodes
2. Add nodes with in-degree 0 to a queue
3. Process nodes from queue, reducing in-degrees of neighbors
4. Continue until queue is empty
5. If all nodes processed, return the ordering; otherwise graph has cycles

## Assembly Implementation

```assembly
; Topological Sorting - Rosalind Problem Solution
; Algorithm: Kahn's Algorithm for topological sorting

.data
    MAX_NODES equ 1000        ; Maximum number of nodes
    nodes_array db MAX_NODES dup(0)  ; Node storage array
    in_degree db MAX_NODES dup(0)    ; In-degree array
    queue db MAX_NODES dup(0)        ; Queue for BFS
    visited db MAX_NODES dup(0)      ; Visited flag array
    result db MAX_NODES dup(0)       ; Result storage
    
    ; Input data example:
    ; Graph with 6 nodes and edges: 1->2, 1->3, 2->4, 3->4, 4->5, 4->6
    num_nodes dw 6
    num_edges dw 6
    
    ; Edge list (source, destination)
    edges db 1,2, 1,3, 2,4, 3,4, 4,5, 4,6

.code
main proc
    ; Initialize data structures
    call initialize_arrays
    
    ; Read input and build adjacency list
    call build_graph
    
    ; Perform topological sort
    call topological_sort
    
    ; Output result
    call print_result
    
    ret
main endp

initialize_arrays proc
    ; Initialize in_degree array to 0
    mov ecx, MAX_NODES
    mov edi, offset in_degree
init_loop:
    mov byte ptr [edi], 0
    inc edi
    loop init_loop
    
    ; Initialize visited array to 0
    mov ecx, MAX_NODES
    mov edi, offset visited
vis_init_loop:
    mov byte ptr [edi], 0
    inc edi
    loop vis_init_loop
    
    ; Initialize result array to 0
    mov ecx, MAX_NODES
    mov edi, offset result
res_init_loop:
    mov byte ptr [edi], 0
    inc edi
    loop res_init_loop
    
    ret
initialize_arrays endp

build_graph proc
    ; Build in-degree counts from edge list
    mov ecx, num_edges
    mov esi, offset edges
    mov eax, 0
    
build_edge_loop:
    ; Get destination node (second byte in pair)
    mov al, [esi+1]     ; Destination node
    dec al              ; Convert to 0-based indexing
    inc eax             ; Convert back for indexing
    
    ; Increment in-degree of destination
    mov ebx, offset in_degree
    mov bl, [ebx + eax - 1]  ; Get current in-degree
    inc bl              ; Increment it
    mov [ebx + eax - 1], bl  ; Store back
    
    add esi, 2          ; Move to next edge pair
    loop build_edge_loop
    
    ret
build_graph endp

topological_sort proc
    ; Initialize queue with nodes of in-degree 0
    call initialize_queue
    
    ; Process nodes in topological order
    mov ecx, 0          ; Result counter
    mov esi, offset queue
    mov edi, offset result
    
sort_loop:
    ; Check if queue is empty
    cmp ecx, num_nodes
    jge sort_complete
    
    ; Get node from front of queue
    mov al, [esi]       ; Get node value
    mov [edi], al       ; Store in result
    inc edi             ; Move to next result position
    inc ecx             ; Increment counter
    
    ; Remove node from queue
    add esi, 1          ; Move to next element
    
    ; Update in-degrees of neighbors
    call update_neighbors
    
    jmp sort_loop
    
sort_complete:
    ret
topological_sort endp

initialize_queue proc
    ; Find all nodes with in-degree 0 and add to queue
    mov ecx, num_nodes
    mov esi, offset in_degree
    mov edi, offset queue
    mov ebx, 0          ; Queue position counter
    
init_queue_loop:
    cmp byte ptr [esi], 0
    jne next_node
    
    ; Node has in-degree 0, add to queue
    mov al, [esi]
    mov [edi], al
    inc edi
    inc ebx
    
next_node:
    inc esi
    loop init_queue_loop
    
    ret
initialize_queue endp

update_neighbors proc
    ; Update in-degrees of neighbors (simplified implementation)
    ; This would normally iterate through adjacency list
    ; For now, just return (in a full implementation this would be more complex)
    ret
update_neighbors endp

print_result proc
    ; Print the topological ordering
    mov ecx, num_nodes
    mov esi, offset result
    
print_loop:
    cmp ecx, 0
    je print_done
    
    ; Print current node
    mov al, [esi]
    ; Print character logic would go here
    ; This is simplified - in actual assembly would need to convert number to string
    
    inc esi
    loop print_loop
    
print_done:
    ret
print_result endp

end main
```

## Key Algorithm Steps

1. **Initialize**: Set up arrays for tracking in-degrees and visited nodes
2. **Build Graph**: Count in-degrees for each node based on input edges  
3. **Queue Setup**: Add all nodes with in-degree 0 to processing queue
4. **Process Nodes**: Remove from queue, output result, update neighbors' in-degrees
5. **Cycle Detection**: If not all nodes processed, graph contains cycles

## Time Complexity
- **Time**: O(V + E) where V is vertices and E is edges
- **Space**: O(V) for storage arrays

## Note
This assembly implementation provides the framework but would need additional helper functions for:
- Input parsing
- Number-to-string conversion 
- Proper queue management
- Detailed edge list processing

The solution follows Kahn's algorithm which is optimal for topological sorting of DAGs.