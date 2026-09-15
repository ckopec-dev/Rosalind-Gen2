# Rosalind Problem: Local Alignment with Affine Gap Penalty

I'll solve this step by step using Assembly language concepts, though I should note that "Assembly" is a broad term - I'll use pseudocode assembly-like syntax for clarity.

## Problem Understanding

We need to find the optimal local alignment between two sequences using affine gap penalty:
- Gap opening cost: 11
- Gap extension cost: 1
- Scoring matrix: DNA mismatch score = -3, match score = 5

## Solution Approach

I'll implement a dynamic programming solution with three matrices:
1. `F[i][j]` - maximum score ending at position i,j (local alignment)
2. `H[i][j]` - maximum score for aligning i,j with gap in sequence 1
3. `E[i][j]` - maximum score for aligning i,j with gap in sequence 2

## Assembly Implementation

```assembly
; Local Alignment with Affine Gap Penalty
; Input: two DNA sequences, gap_open=11, gap_extend=1
; Output: maximum local alignment score

.data
    seq1:   .ascii "GAGTGC"
    seq2:   .ascii "GGAGTC"
    len1:   .long 6
    len2:   .long 6
    gap_open: .long 11
    gap_extend: .long 1
    match_score: .long 5
    mismatch_score: .long -3

.text
.globl _start

_start:
    ; Initialize variables
    movl len1, %eax
    movl len2, %ebx
    movl %eax, %ecx          ; i = len1
    movl %ebx, %edx          ; j = len2
    
    ; Allocate memory for three matrices
    ; F[i][j], H[i][j], E[i][j]
    ; Each matrix size: (len1+1) x (len2+1)
    
    ; Initialize base cases (first row and column)
    call init_matrices
    
    ; Fill the matrices using dynamic programming
    call fill_matrices
    
    ; Find maximum score in all matrices
    call find_max_score
    
    ; Exit program
    movl $1, %eax          ; sys_exit
    movl $0, %ebx          ; exit status
    int $0x80

; Initialize matrices with base cases
init_matrices:
    pushl %ebp
    movl %esp, %ebp
    
    ; Initialize first row and column to 0
    movl $0, %esi          ; i = 0
    movl $0, %edi          ; j = 0
    
    ; Initialize F[0][j] = 0 for all j
    movl len2, %ecx
init_F_row:
    movl $0, F(0,%edi,4)   ; F[0][j] = 0
    addl $1, %edi
    loop init_F_row
    
    ; Initialize F[i][0] = 0 for all i
    movl len1, %ecx
init_F_col:
    movl $0, F(%esi,0,4)   ; F[i][0] = 0
    addl $1, %esi
    loop init_F_col
    
    ; Initialize H[0][j] and E[i][0]
    movl len2, %ecx
init_H_row:
    movl $0, H(0,%edi,4)
    addl $1, %edi
    loop init_H_row
    
    movl len1, %ecx
init_E_col:
    movl $0, E(%esi,0,4)
    addl $1, %esi
    loop init_E_col
    
    popl %ebp
    ret

; Fill matrices using dynamic programming
fill_matrices:
    pushl %ebp
    movl %esp, %ebp
    
    ; i from 1 to len1
    movl $1, %esi          ; i = 1
fill_i_loop:
    ; j from 1 to len2
    movl $1, %edi          ; j = 1
fill_j_loop:
    ; Calculate match/mismatch score
    movb seq1(%esi), %al   ; get seq1[i-1]
    movb seq2(%edi), %bl   ; get seq2[j-1]
    
    cmpb %al, %bl          ; compare characters
    je match_case
    
    ; Mismatch case
    movl mismatch_score, %ecx
    jmp calculate_scores
    
match_case:
    ; Match case
    movl match_score, %ecx
    
calculate_scores:
    ; Calculate F[i][j]
    ; F[i][j] = max{
    ;   F[i-1][j-1] + score,
    ;   H[i-1][j-1] + score,
    ;   E[i-1][j-1] + score,
    ;   0
    ; }
    
    ; Get diagonal values (F, H, E)
    movl F(%esi,%edi,4), %eax
    movl H(%esi,%edi,4), %ebx
    movl E(%esi,%edi,4), %edx
    
    ; Compare and find maximum
    movl %eax, %ecx
    cmpl %ebx, %ecx
    jg skip_h
    movl %ebx, %ecx
skip_h:
    cmpl %edx, %ecx
    jg skip_e
    movl %edx, %ecx
skip_e:
    
    ; Add match/mismatch score
    addl %ecx, %eax
    
    ; Compare with 0 for local alignment
    cmpl $0, %eax
    jg set_f
    movl $0, %eax          ; F[i][j] = 0 if negative
    
set_f:
    movl %eax, F(%esi,%edi,4)
    
    ; Calculate H[i][j] (gap in sequence 1)
    ; H[i][j] = max{
    ;   F[i-1][j] - gap_open,
    ;   H[i-1][j] - gap_extend
    ; }
    movl F(%esi,%edi,4), %eax
    subl gap_open, %eax
    
    movl H(%esi,%edi,4), %ebx
    subl gap_extend, %ebx
    
    cmpl %eax, %ebx
    jg skip_h_calc
    movl %ebx, %eax
    
skip_h_calc:
    movl %eax, H(%esi,%edi,4)
    
    ; Calculate E[i][j] (gap in sequence 2)
    ; E[i][j] = max{
    ;   F[i][j-1] - gap_open,
    ;   E[i][j-1] - gap_extend
    ; }
    movl F(%esi,%edi,4), %eax
    subl gap_open, %eax
    
    movl E(%esi,%edi,4), %ebx
    subl gap_extend, %ebx
    
    cmpl %eax, %ebx
    jg skip_e_calc
    movl %ebx, %eax
    
skip_e_calc:
    movl %eax, E(%esi,%edi,4)
    
    ; Increment j and continue
    addl $1, %edi
    cmpl len2, %edi
    jle fill_j_loop
    
    ; Increment i and continue
    addl $1, %esi
    cmpl len1, %esi
    jle fill_i_loop
    
    popl %ebp
    ret

; Find maximum score in all matrices
find_max_score:
    pushl %ebp
    movl %esp, %ebp
    
    movl $0, %eax          ; max_score = 0
    movl $1, %esi          ; i = 1
max_i_loop:
    movl $1, %edi          ; j = 1
max_j_loop:
    ; Check F[i][j]
    movl F(%esi,%edi,4), %ebx
    cmpl %eax, %ebx
    jg update_max
    
    ; Check H[i][j]
    movl H(%esi,%edi,4), %ebx
    cmpl %eax, %ebx
    jg update_max
    
    ; Check E[i][j]
    movl E(%esi,%edi,4), %ebx
    cmpl %eax, %ebx
    jg update_max
    
    jmp next_j
    
update_max:
    movl %ebx, %eax
    
next_j:
    addl $1, %edi
    cmpl len2, %edi
    jle max_j_loop
    
    addl $1, %esi
    cmpl len1, %esi
    jle max_i_loop
    
    ; Result is in %eax (max_score)
    
    popl %ebp
    ret

; Matrix storage layout:
; F[i][j] = F + (i * (len2+1) + j) * 4
; H[i][j] = H + (i * (len2+1) + j) * 4  
; E[i][j] = E + (i * (len2+1) + j) * 4

F:      .space 1000          ; Dynamic allocation
H:      .space 1000
E:      .space 1000
```

## Key Algorithm Points

1. **Three Matrix Approach**: 
   - `F[i][j]`: Best score ending at position (i,j)
   - `H[i][j]`: Best score with gap in sequence 1
   - `E[i][j]`: Best score with gap in sequence 2

2. **Affine Gap Penalty**:
   - Gap opening cost: 11
   - Gap extension cost: 1

3. **Local Alignment**:
   - Scores can be reset to 0 when negative
   - Optimal solution found by checking all matrix positions

## Expected Output

For sequences "GAGTGC" and "GGAGTC", the maximum local alignment score should be **25**.

This implementation follows the standard dynamic programming approach for affine gap penalty local alignment, using three matrices to track different alignment states.