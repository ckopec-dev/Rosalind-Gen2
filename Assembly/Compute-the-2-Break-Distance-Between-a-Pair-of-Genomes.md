# Rosalind Problem: Compute the 2-Break Distance Between a Pair of Genomes

## Problem Understanding

The 2-break distance between two genomes is the minimum number of 2-break operations needed to transform one genome into another. A 2-break operation splits two edges and creates two new edges, effectively rearranging genome structure.

## Approach

1. Convert genomes to adjacency lists
2. Find cycles in the breakpoint graph
3. Calculate distance using the formula: d = n - c where n is the number of vertices and c is the number of cycles

## Solution in Assembly

```assembly
; Compute 2-break distance between two genomes
; Input: Two genomes as lists of signed integers
; Output: The 2-break distance

; Global variables
genome1: .word 1, -3, -8, 4, 2, 5, -6, 7
genome2: .word 1, 2, 3, 4, 5, 6, 7, 8
len1: .word 8
len2: .word 8

; Main function
compute_distance:
    ; Get lengths of genomes
    lw $t0, len1
    lw $t1, len2
    
    ; Calculate number of vertices (2 * n where n is number of genes)
    sll $t2, $t0, 1      ; $t2 = 2 * len1
    
    ; Create adjacency lists for both genomes
    ; This is a simplified version - in practice would need full implementation
    ; of adjacency list creation
    
    ; Find cycles in breakpoint graph
    ; For each cycle found, decrement count
    li $t3, 0            ; cycle count
    
    ; Calculate distance: d = n - c
    sub $v0, $t2, $t3    ; distance = vertices - cycles
    
    jr $ra

; Helper function to create adjacency list
create_adjacency:
    ; Parameters: genome pointer, length
    ; Returns: adjacency list structure
    ; (Implementation would be more complex)
    jr $ra

; Helper function to find cycles
find_cycles:
    ; Parameters: adjacency lists
    ; Returns: number of cycles
    ; (Implementation would use graph traversal algorithms)
    jr $ra

; Alternative approach using cycle counting
compute_distance_cycles:
    ; Get genome lengths
    lw $t0, len1
    lw $t1, len2
    
    ; Total vertices = 2 * number of genes
    add $t2, $t0, $t0    ; $t2 = 2 * len1
    
    ; Count cycles in breakpoint graph
    jal find_cycles
    
    ; Distance = vertices - cycles
    sub $v0, $t2, $v1    ; $v0 = vertices - cycles
    
    jr $ra

; Example usage
main:
    jal compute_distance
    
    ; Print result
    li $v0, 1            ; sys_write
    li $a0, 1            ; stdout
    move $a1, $v0        ; result
    li $a2, 4            ; length
    syscall
    
    li $v0, 10           ; exit
    syscall
```

## Detailed Algorithm

```assembly
; More complete implementation
compute_2break_distance:
    ; Input: two genome arrays, their lengths
    ; Output: 2-break distance
    
    ; Step 1: Convert genomes to adjacency representation
    ; Step 2: Build breakpoint graph
    ; Step 3: Count cycles
    ; Step 4: Apply formula
    
    ; Initialize variables
    lw $t0, len1         ; genome1 length
    lw $t1, len2         ; genome2 length
    
    ; Total vertices = 2 * number of genes
    add $t2, $t0, $t0    ; $t2 = 2 * len1
    
    ; Count cycles in breakpoint graph
    jal count_cycles
    
    ; Distance = total_vertices - cycles
    sub $v0, $t2, $v1    ; result = vertices - cycles
    
    jr $ra

count_cycles:
    ; This would implement cycle detection in the breakpoint graph
    ; For a simple case with 8 genes:
    ; - Each gene contributes 2 vertices
    ; - Total vertices = 16
    ; - Count connected components in the breakpoint graph
    ; - Number of cycles = components - 1
    
    ; Simplified for demonstration
    li $v1, 0            ; initialize cycle count
    jr $ra
```

## Mathematical Formula

The 2-break distance is calculated using:
```
d = n - c
```
Where:
- n = number of vertices in the breakpoint graph = 2 × (number of genes)
- c = number of cycles in the breakpoint graph

## Time Complexity
- O(n) where n is the number of genes
- Space complexity: O(n)

## Key Points
1. Convert genomes to adjacency lists
2. Build the breakpoint graph
3. Count connected components/cycles
4. Apply the mathematical formula to get the distance

This implementation provides the framework for solving the 2-break distance problem using assembly language concepts, though a complete implementation would require detailed graph traversal algorithms and memory management.

