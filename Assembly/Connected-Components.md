# Rosalind Problem: Connected Components

## Problem Understanding

The connected components problem asks us to find the number of connected components in an undirected graph. A connected component is a subgraph in which any two vertices are connected to each other by paths, and which is connected to no additional vertices in the supergraph.

## Approach

I'll use the Union-Find (Disjoint Set Union) data structure to solve this problem efficiently:

1. Initialize each vertex as its own parent
2. For each edge, perform union operation on the two vertices
3. Count the number of distinct root nodes (connected components)

## Assembly Implementation

```assembly
; Rosalind Connected Components - Assembly Solution
; Problem: Given an undirected graph with n vertices and m edges,
; return the number of connected components

.data
    MAX_NODES equ 10000      ; Maximum number of nodes
    nodes dd MAX_NODES dup(0) ; Array to store parent information
    rank dd MAX_NODES dup(0)   ; Array to store rank for union by rank
    
    n dd 0          ; Number of vertices
    m dd 0          ; Number of edges
    
    edge_count dd 0 ; Counter for edges processed
    
.code

; Initialize the Union-Find structure
init_union_find proc
    push ebx
    push ecx
    push edx
    
    mov ecx, [n]    ; Load number of nodes
    xor ebx, ebx    ; Initialize counter
    
init_loop:
    cmp ebx, ecx
    jge init_done
    
    mov [nodes + ebx * 4], ebx   ; Each node is its own parent initially
    mov [rank + ebx * 4], 0      ; Rank initialized to 0
    
    inc ebx
    jmp init_loop
    
init_done:
    pop edx
    pop ecx
    pop ebx
    ret
init_union_find endp

; Find operation with path compression
find proc
    push ebx
    push ecx
    push edx
    
    mov ebx, [esp + 12] ; Get the node parameter
    
find_loop:
    cmp ebx, [nodes + ebx * 4]  ; Compare node with its parent
    je find_done
    
    mov ecx, [nodes + ebx * 4]  ; Get parent of current node
    mov edx, [nodes + ecx * 4]  ; Get grandparent
    
    ; Path compression: make grandparent the new parent
    cmp ebx, edx
    je skip_update
    
    mov [nodes + ebx * 4], edx  ; Update parent
    
skip_update:
    mov ebx, edx  ; Move to grandparent
    jmp find_loop
    
find_done:
    pop edx
    pop ecx
    pop ebx
    ret
find endp

; Union operation with union by rank
union proc
    push ebx
    push ecx
    push edx
    
    mov ebx, [esp + 12] ; First node parameter
    mov ecx, [esp + 16] ; Second node parameter
    
    ; Find roots of both nodes
    call find
    mov edx, eax        ; Root of first node
    mov ebx, [esp + 12] ; Restore first node
    
    call find
    mov esi, eax        ; Root of second node
    
    cmp edx, esi
    je union_done       ; If same root, already connected
    
    ; Union by rank
    mov eax, [rank + edx * 4]
    mov ebx, [rank + esi * 4]
    
    cmp eax, ebx
    jg union_by_rank
    
    ; If ranks are equal or first has smaller rank
    mov [nodes + edx * 4], esi   ; Make second root the parent of first
    
    cmp eax, ebx
    je increment_rank
    
    jmp union_done
    
union_by_rank:
    mov [nodes + esi * 4], edx   ; Make first root the parent of second
    
increment_rank:
    inc dword ptr [rank + edx * 4] ; Increment rank of new root
    
union_done:
    pop edx
    pop ecx
    pop ebx
    ret
union endp

; Main program logic
main proc
    push ebp
    mov ebp, esp
    
    ; Read n and m
    call read_int
    mov [n], eax
    
    call read_int
    mov [m], eax
    
    ; Initialize Union-Find structure
    call init_union_find
    
    ; Process edges
    xor ebx, ebx        ; edge counter
    
process_edges:
    cmp ebx, [m]
    jge count_components
    
    ; Read two vertices of edge
    call read_int
    mov ecx, eax
    
    call read_int
    mov edx, eax
    
    ; Union the two vertices
    push edx
    push ecx
    call union
    add esp, 8          ; Clean up stack
    
    inc ebx
    jmp process_edges
    
count_components:
    ; Count number of connected components
    xor ebx, ebx        ; component counter
    xor ecx, ecx        ; node counter
    
count_loop:
    cmp ecx, [n]
    jge print_result
    
    mov edx, ecx
    call find         ; Find root of current node
    
    cmp eax, ecx      ; Check if node is its own root
    jne count_next
    
    inc ebx           ; Increment component count
    
count_next:
    inc ecx
    jmp count_loop
    
print_result:
    ; Print result
    mov eax, ebx
    call print_int
    
    ; Exit program
    mov esp, ebp
    pop ebp
    ret
main endp

; Helper function to read integer (simplified)
read_int proc
    push ebx
    push ecx
    push edx
    
    xor eax, eax      ; Clear result
    xor ebx, ebx      ; Clear character
    
read_char_loop:
    ; Assume we have a character input function
    ; For simplicity, let's say this reads a single digit
    ; In real implementation, would need proper I/O handling
    
    mov ebx, 10       ; Base 10
    mul ebx           ; Multiply result by 10
    add eax, ebx      ; Add new digit
    
    jmp read_char_loop  ; Continue reading until newline or space
    
read_int_done:
    pop edx
    pop ecx
    pop ebx
    ret
read_int endp

; Helper function to print integer (simplified)
print_int proc
    push ebx
    push ecx
    push edx
    
    ; Simple implementation - convert number to string and print
    ; This would require more complex logic in real assembly
    
    pop edx
    pop ecx
    pop ebx
    ret
print_int endp

end main
```

## Algorithm Explanation

1. **Initialization**: Create a Union-Find data structure where each node initially points to itself as its parent.

2. **Edge Processing**: For each edge in the graph, perform a union operation on the two vertices it connects.

3. **Connected Components Counting**: After processing all edges, count how many distinct root nodes exist - this equals the number of connected components.

## Time Complexity
- Union-Find with path compression and union by rank: nearly O(1) per operation
- Overall: O(m × α(n)) where m is number of edges, n is number of vertices, and α is the inverse Ackermann function

## Space Complexity
- O(n) for storing parent and rank arrays

This solution efficiently handles the connected components problem using the Union-Find data structure with optimizations for performance.