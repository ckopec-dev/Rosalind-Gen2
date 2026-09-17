# Rosalind Problem: Degree Array

## Problem Description
Given an undirected graph with n nodes and m edges, return the degree of each node.

The degree of a node is the number of edges incident to it.

## Solution Approach
1. Read the number of nodes (n) and edges (m)
2. Initialize an array to store degrees of all nodes
3. For each edge, increment the degree count for both nodes involved
4. Output the degree of each node

## Assembly Implementation

```assembly
.data
    n DWORD ?           ; number of nodes
    m DWORD ?           ; number of edges
    degrees DWORD 10000 DUP(0)  ; array to store degrees (max 10000 nodes)
    
.text
main PROC
    ; Read n and m
    mov eax, n
    mov ebx, m
    call read_int
    mov n, eax
    mov m, ebx
    
    ; Initialize degrees array to 0
    xor ecx, ecx        ; counter
init_loop:
    cmp ecx, n
    jge init_done
    mov degrees[ecx*4], 0
    inc ecx
    jmp init_loop
init_done:
    
    ; Read edges and update degrees
    xor ecx, ecx        ; edge counter
edge_loop:
    cmp ecx, m
    jge edge_done
    
    ; Read two nodes for current edge
    call read_int
    mov esi, eax        ; first node
    call read_int
    mov edi, eax        ; second node
    
    ; Increment degrees for both nodes
    inc degrees[esi*4]
    inc degrees[edi*4]
    
    inc ecx
    jmp edge_loop
edge_done:
    
    ; Output degrees for all nodes
    xor ecx, ecx        ; node counter
output_loop:
    cmp ecx, n
    jge output_done
    
    mov eax, degrees[ecx*4]
    call print_int
    call print_newline
    
    inc ecx
    jmp output_loop
output_done:
    
    ret
main ENDP

; Helper function to read integer (simplified)
read_int PROC
    ; This would contain actual input reading logic
    ; For demonstration, we'll assume it loads a value into eax
    ; In practice, this would use system calls or input routines
    mov eax, 0          ; placeholder - actual implementation needed
    ret
read_int ENDP

; Helper function to print integer (simplified)
print_int PROC
    ; This would contain actual output logic
    ; For demonstration, we'll assume it prints the value in eax
    ret
print_int ENDP

; Helper function to print newline (simplified)
print_newline PROC
    ; This would output a newline character
    ret
print_newline ENDP
```

## Key Points

1. **Data Structure**: Uses an array `degrees` to store the degree of each node
2. **Initialization**: All degrees are initialized to 0 before processing edges
3. **Edge Processing**: For each edge connecting nodes u and v, increment both `degrees[u]` and `degrees[v]`
4. **Output**: Print degrees for all nodes from 1 to n

## Time Complexity
- O(n + m) where n is the number of nodes and m is the number of edges
- O(n) for initialization
- O(m) for processing edges

## Space Complexity
- O(n) for storing the degree array

Note: This implementation assumes the existence of helper functions for input/output operations, which would need to be implemented based on the specific assembly environment being used.