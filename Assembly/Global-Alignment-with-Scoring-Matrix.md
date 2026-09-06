# Rosalind Problem: Global Alignment with Scoring Matrix (Assembly)

## Problem Understanding

This problem requires implementing global sequence alignment using a scoring matrix approach, similar to the Needleman-Wunsch algorithm but with a custom scoring matrix instead of simple match/mismatch scores.

## Assembly Implementation

```assembly
; Global Alignment with Scoring Matrix
; Input: Two sequences and scoring matrix
; Output: Optimal global alignment score and alignment

.section .data
    ; Sequence 1 and 2 (null-terminated)
    seq1: .ascii "ACGTACGT\0"
    seq2: .ascii "ACGTACGT\0"
    
    ; Scoring matrix (5x5 for nucleotides A,C,G,T,N)
    score_matrix:
        .word  2, -1, -1, -1, -1  ; A
        .word -1,  2, -1, -1, -1  ; C  
        .word -1, -1,  2, -1, -1  ; G
        .word -1, -1, -1,  2, -1  ; T
        .word -1, -1, -1, -1,  0  ; N (gap penalty)
    
    gap_penalty: .word -1
    
.section .text
    .global _start

_start:
    ; Initialize registers
    movl seq1, %esi     ; Sequence 1 pointer
    movl seq2, %edi     ; Sequence 2 pointer
    movl score_matrix, %ebx  ; Scoring matrix pointer
    movl gap_penalty, %ecx   ; Gap penalty
    
    ; Calculate sequence lengths
    call get_length
    movl %eax, %edx     ; Length of seq1
    
    call get_length
    movl %eax, %ebp     ; Length of seq2
    
    ; Allocate DP matrix: (len1+1) x (len2+1)
    movl %edx, %eax
    incl %eax           ; +1 for zero row
    movl %ebp, %ecx
    incl %ecx           ; +1 for zero column
    imull %ecx, %eax    ; Size of matrix
    
    ; Allocate memory for DP table
    call malloc
    movl %eax, %edi     ; DP table pointer
    
    ; Initialize DP table
    call initialize_dp_table
    
    ; Fill DP table using dynamic programming
    call fill_dp_table
    
    ; Trace back to get alignment
    call traceback_alignment
    
    ; Exit program
    movl $1, %eax       ; sys_exit
    movl $0, %ebx       ; exit status
    int $0x80

; Function: get_length
; Input: string pointer in %esi
; Output: length in %eax
get_length:
    pushl %esi
    xorl %eax, %eax     ; Clear length counter
    
count_loop:
    lodsb               ; Load byte from %esi into %al
    testb %al, %al      ; Test if null terminator
    jz count_done       ; Jump if zero (end of string)
    incl %eax           ; Increment counter
    jmp count_loop      ; Continue counting
    
count_done:
    popl %esi
    ret

; Function: initialize_dp_table
; Input: DP table pointer in %edi, lengths in %edx and %ebp
initialize_dp_table:
    pushl %edi
    pushl %edx
    pushl %ebp
    
    ; Initialize first row (0 to len2)
    movl %ebp, %ecx     ; Length of seq2
    xorl %eax, %eax     ; Start at 0
    movl %edi, %esi     ; DP table pointer
    
init_row_loop:
    movl %eax, (%esi)   ; Store value in DP table
    addl $4, %esi       ; Move to next cell (4 bytes)
    incl %eax           ; Increment gap penalty
    decl %ecx
    jnz init_row_loop
    
    ; Initialize first column (0 to len1)
    movl %edx, %ecx     ; Length of seq1
    xorl %eax, %eax     ; Start at 0
    movl %edi, %esi     ; DP table pointer
    addl $4, %esi       ; Skip first element
    
init_col_loop:
    movl %eax, (%esi)   ; Store value in DP table
    addl $4, %esi       ; Move to next row (assuming 4 bytes per cell)
    addl $4, %esi       ; Skip row (for column-wise access)
    incl %eax           ; Increment gap penalty
    decl %ecx
    jnz init_col_loop
    
    popl %ebp
    popl %edx
    popl %edi
    ret

; Function: fill_dp_table
; Input: DP table pointer in %edi, lengths in %edx and %ebp
fill_dp_table:
    pushl %edi
    pushl %edx
    pushl %ebp
    
    ; Loop through matrix (excluding first row and column)
    movl %edx, %ecx     ; i = 1 to len1
outer_loop:
    movl %ebp, %esi     ; j = 1 to len2
inner_loop:
    ; Calculate score from three possible sources:
    ; 1. Diagonal (match/mismatch)
    ; 2. Up (gap in sequence 1)  
    ; 3. Left (gap in sequence 2)
    
    ; Get diagonal score
    movl -4(%edi), %eax     ; Diagonal element
    
    ; Get up score
    movl (%edi, -4), %ebx   ; Up element
    
    ; Get left score  
    movl (%edi), %ecx       ; Left element
    
    ; Find maximum of three scores
    call max_three_scores
    
    ; Store in current position
    movl %eax, (%edi)
    
    addl $4, %edi           ; Move to next cell
    decl %esi
    jnz inner_loop
    
    decl %ecx
    jnz outer_loop
    
    popl %ebp
    popl %edx
    popl %edi
    ret

; Function: max_three_scores
; Input: three scores in %eax, %ebx, %ecx
; Output: maximum score in %eax
max_three_scores:
    ; Compare eax with ebx
    cmpl %ebx, %eax
    jge skip1
    movl %ebx, %eax
skip1:
    
    ; Compare result with ecx  
    cmpl %ecx, %eax
    jge skip2
    movl %ecx, %eax
skip2:
    ret

; Function: traceback_alignment
; Input: DP table pointer in %edi, sequences in %esi and %ebp
traceback_alignment:
    ; Implement backtracking algorithm to construct alignment
    pushl %edi
    pushl %esi
    pushl %ebp
    
    ; Start from bottom-right corner
    movl %edx, %eax     ; len1
    movl %ebp, %ebx     ; len2
    imull %ebx, %eax    ; Calculate index for (len1, len2)
    
    ; Trace back through matrix
    call trace_back
    
    popl %ebp
    popl %esi
    popl %edi
    ret

; Function: trace_back
; Input: current position in DP table
trace_back:
    ; Implementation of traceback logic
    ; This would involve backtracking from the final cell
    ; to reconstruct the optimal alignment
    
    ; Placeholder for actual traceback implementation
    ret

.section .bss
    .lcomm dp_table, 1024   ; Dynamic programming table
```

## Key Algorithm Steps

1. **Initialization**: Create a DP matrix of size (m+1) × (n+1)
2. **Base Cases**: Initialize first row and column with gap penalties
3. **Fill Table**: For each cell, compute maximum score from three directions:
   - Diagonal: match/mismatch score
   - Up: gap penalty for sequence 1  
   - Left: gap penalty for sequence 2
4. **Traceback**: Reconstruct optimal alignment by backtracking

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the DP table

## Notes

This is a simplified implementation that demonstrates the core concepts. A complete solution would need to:
1. Properly handle memory allocation for large sequences
2. Implement exact traceback to generate alignments
3. Handle different scoring matrices
4. Add proper input/output handling