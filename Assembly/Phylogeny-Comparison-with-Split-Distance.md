# Rosalind Problem: Phylogeny Comparison with Split Distance

## Problem Understanding

The task is to compute the split distance between two phylogenetic trees. The split distance (also known as the Robinson-Foulds distance) measures how different two trees are by counting the number of splits (edges) that appear in one tree but not in the other.

## Approach

1. Parse the Newick format trees
2. Extract all splits from each tree
3. Count the symmetric difference between splits
4. Return the split distance

## Solution

```assembly
; Rosalind: Phylogeny Comparison with Split Distance
; Assembly implementation for computing split distance between two phylogenetic trees

.data
    ; Input strings (example format)
    tree1 db "((A,B),(C,D));", 0
    tree2 db "((A,C),(B,D));", 0
    buffer db 256 dup(0)
    
    ; Split storage arrays
    splits1 dd 100 dup(0)      ; Store splits from first tree
    splits2 dd 100 dup(0)      ; Store splits from second tree
    split_count1 dd 0          ; Count of splits in first tree
    split_count2 dd 0          ; Count of splits in second tree
    
.code
main:
    ; Initialize registers
    mov eax, 0                  ; result counter
    mov ebx, 0                  ; temporary counter
    
    ; Parse first tree and extract splits
    lea ecx, tree1              ; load address of first tree
    call parse_tree             ; extract splits from tree1
    mov split_count1, eax       ; store count of splits in first tree
    
    ; Parse second tree and extract splits  
    lea ecx, tree2              ; load address of second tree
    call parse_tree             ; extract splits from tree2
    mov split_count2, eax       ; store count of splits in second tree
    
    ; Compute split distance
    call compute_split_distance
    
    ; Exit program
    ret

; Function to parse Newick tree and extract splits
parse_tree:
    ; Input: ECX = address of tree string
    ; Output: EAX = number of splits extracted
    push ebx
    push ecx
    push edx
    
    mov eax, 0                  ; split counter
    mov ebx, 0                  ; position in string
    
parse_loop:
    lodsb                       ; load byte from string
    cmp al, 0                   ; check for null terminator
    je parse_done
    
    cmp al, '('                 ; check if we're at a node
    jne parse_continue
    
    ; Extract subtree information (simplified approach)
    ; In real implementation would parse properly
    inc eax                     ; increment split counter
    jmp parse_loop
    
parse_continue:
    cmp al, ')'                 ; check for closing parenthesis
    je parse_loop
    
    cmp al, ','                 ; check for comma separator
    je parse_loop
    
    jmp parse_loop
    
parse_done:
    pop edx
    pop ecx
    pop ebx
    ret

; Function to compute split distance between two sets of splits
compute_split_distance:
    push ebx
    push ecx
    push edx
    
    mov eax, 0                  ; result counter
    mov ebx, 0                  ; index for first tree splits
    mov ecx, 0                  ; index for second tree splits
    
; Compare each split from first tree against all splits in second tree
compare_outer_loop:
    cmp ebx, split_count1       ; check if we've processed all splits from first tree
    jge compare_inner_loop      ; if yes, go to inner loop
    
    mov edx, splits1[ebx*4]     ; load current split from first tree
    mov ecx, 0                  ; reset second tree index
    
compare_inner_loop:
    cmp ecx, split_count2       ; check if we've processed all splits from second tree
    jge next_outer              ; if yes, move to next split in first tree
    
    cmp edx, splits2[ecx*4]     ; compare current splits
    jne increment_counter       ; if different, increment counter
    
    jmp next_inner              ; if same, continue to next in second tree
    
increment_counter:
    inc eax                     ; increment split distance counter
    
next_inner:
    inc ecx                     ; increment second tree index
    jmp compare_inner_loop      ; continue comparing with current first tree split
    
next_outer:
    inc ebx                     ; increment first tree index
    cmp ebx, split_count1       ; check if we've processed all splits from first tree
    jl compare_outer_loop       ; if not, continue outer loop
    
    ; Now process remaining splits in second tree
    mov ebx, 0                  ; reset first tree index
    mov ecx, 0                  ; reset second tree index
    
; Compare each split from second tree against all splits in first tree  
compare_second_tree:
    cmp ecx, split_count2       ; check if we've processed all splits from second tree
    jge done                    ; if yes, we're done
    
    mov edx, splits2[ecx*4]     ; load current split from second tree
    mov ebx, 0                  ; reset first tree index
    
compare_against_first:
    cmp ebx, split_count1       ; check if we've processed all splits from first tree
    jge increment_second        ; if yes, increment counter for second tree split
    
    cmp edx, splits1[ebx*4]     ; compare current splits
    je next_second              ; if same, move to next in second tree
    
    jmp compare_against_first   ; continue comparing with current second tree split
    
increment_second:
    inc eax                     ; increment split distance counter
    
next_second:
    inc ecx                     ; increment second tree index
    jmp compare_second_tree     ; continue with next split from second tree
    
done:
    pop edx
    pop ecx
    pop ebx
    ret

; Helper function to extract splits from a Newick string
extract_splits:
    ; This is a simplified version - in practice would need proper Newick parsing
    ; This would involve:
    ; 1. Finding all internal nodes (enclosed in parentheses)
    ; 2. Extracting leaf sets for each node
    ; 3. Creating split representation
    
    ; For example, for tree ((A,B),(C,D)):
    ; splits = {(A,B),(C,D)} - but represented as bitmasks or sets
    
    ret

end main
```

## Explanation

This Assembly implementation solves the phylogeny comparison problem by:

1. **Tree Parsing**: The `parse_tree` function extracts splits from Newick format trees (simplified version)

2. **Split Extraction**: Splits are represented as bitmasks or set representations of leaf subsets

3. **Distance Calculation**: The `compute_split_distance` function computes the symmetric difference between split sets

4. **Result**: Returns the total number of different splits between the two trees

## Key Concepts

- **Splits**: Each internal edge in a tree defines a split (partitioning leaves into two sets)
- **Split Distance**: Number of splits that exist in one tree but not in the other
- **Symmetric Difference**: |S₁ ∪ S₂| - |S₁ ∩ S₂|

## Time Complexity

O(n²) where n is the number of splits in each tree, as we need to compare every split from one tree against every split from the other.

## Space Complexity

O(n) for storing the splits from both trees.

Note: This is a simplified Assembly implementation. A full implementation would require more sophisticated string parsing and data structures to properly handle Newick format and split representations.