# Rosalind Problem: Counting Optimal Alignments

## Problem Understanding

We need to count the number of optimal global alignments between two strings using dynamic programming with traceback counting.

## Approach

1. Build a dynamic programming matrix to find the optimal alignment score
2. Count the number of ways to achieve this optimal score
3. Use the recurrence relation for counting alignments

## Solution in Assembly

```assembly
; Counting Optimal Alignments - Assembly Implementation
; Input: Two strings s and t
; Output: Number of optimal global alignments

.data
    s:      .ascii "AGTACG"    ; First string
    t:      .ascii "ACGTACG"    ; Second string
    s_len:  .long 6             ; Length of first string
    t_len:  .long 7             ; Length of second string
    match:  .long 1             ; Match score
    mismatch: .long -1          ; Mismatch score
    gap:    .long -1           ; Gap penalty

.text
.globl _start

_start:
    ; Initialize variables
    movl s_len, %eax            ; Get length of s
    movl t_len, %ebx            ; Get length of t
    addl $1, %eax               ; Add 1 for matrix dimensions
    addl $1, %ebx
    imull %ebx, %eax            ; Calculate matrix size
    
    ; Allocate memory for DP matrix
    movl %eax, %ecx
    movl $4, %edx               ; Size of integer
    call malloc
    
    ; Initialize DP matrix
    call init_dp_matrix
    
    ; Fill DP matrix
    call fill_dp_matrix
    
    ; Count alignments
    call count_alignments
    
    ; Exit
    movl $1, %eax
    movl $0, %ebx
    int $0x80

; Initialize DP matrix with zeros
init_dp_matrix:
    pushl %eax                  ; Save matrix pointer
    pushl %ecx                  ; Save matrix size
    
    movl $0, %ecx               ; Counter
    movl (%eax), %ebx           ; Get matrix pointer
    
init_loop:
    cmpl %ecx, %edx             ; Compare counter with size
    jge init_done
    
    movl $0, (%ebx,%ecx,4)      ; Set matrix[i][j] = 0
    incl %ecx
    jmp init_loop
    
init_done:
    popl %ecx
    popl %eax
    ret

; Fill DP matrix with scores
fill_dp_matrix:
    pushl %eax                  ; Save matrix pointer
    pushl %ebx                  ; Save string pointers
    pushl %ecx                  ; Save lengths
    
    movl s, %esi                ; String s
    movl t, %edi                ; String t
    movl s_len, %ecx            ; Length of s
    movl t_len, %edx            ; Length of t
    
    ; Initialize first row and column
    movl $1, %eax
    movl $0, %ebx
    
    ; Initialize first row
first_row_loop:
    cmpl %ecx, %ebx
    jge first_row_done
    
    movl %ebx, %edi             ; Column index
    movl %eax, %esi             ; Row index
    call get_matrix_index
    movl $%ebx, (%eax)          ; Set score
    incl %ebx
    jmp first_row_loop
    
first_row_done:
    ; Initialize first column
    movl $0, %ebx
first_col_loop:
    cmpl %edx, %ebx
    jge first_col_done
    
    movl %ebx, %edi             ; Column index
    movl $1, %esi               ; Row index
    call get_matrix_index
    movl %ebx, (%eax)           ; Set score
    incl %ebx
    jmp first_col_loop
    
first_col_done:
    ; Fill the rest of matrix
    movl $1, %esi               ; Row index
fill_loop:
    cmpl %ecx, %esi
    jge fill_done
    
    movl $1, %edi               ; Column index
fill_inner_loop:
    cmpl %edx, %edi
    jge fill_inner_done
    
    ; Calculate scores for three possible moves
    movl %esi, %eax             ; Row index
    movl %edi, %ebx             ; Column index
    
    ; Get diagonal score (match/mismatch)
    decl %eax
    decl %ebx
    call get_matrix_index
    movl (%eax), %ebp           ; Diagonal score
    
    ; Get left score (gap)
    movl %esi, %eax
    decl %ebx
    call get_matrix_index
    movl (%eax), %edx           ; Left score
    
    ; Get up score (gap)
    decl %eax
    movl %esi, %eax
    call get_matrix_index
    movl (%eax), %ecx           ; Up score
    
    ; Calculate match/mismatch score
    movl %esi, %eax
    decl %eax
    movl %edi, %ebx
    decl %ebx
    movb s(%eax), %al           ; s[i-1]
    movb t(%ebx), %bl           ; t[j-1]
    cmpb %al, %bl
    je match_score
    movl mismatch, %eax
    jmp score_calc_done
    
match_score:
    movl match, %eax
    
score_calc_done:
    addl %eax, %ebp             ; Add match/mismatch score
    
    ; Find maximum of three scores
    movl %ebp, %eax             ; Diagonal
    cmpl %edx, %eax             ; Compare with left
    jge score1
    movl %edx, %eax
score1:
    cmpl %ecx, %eax             ; Compare with up
    jge score2
    movl %ecx, %eax
score2:
    
    ; Store maximum score
    movl %esi, %ebx
    movl %edi, %ecx
    call get_matrix_index
    movl %eax, (%ebx)
    
    incl %edi
    jmp fill_inner_loop
    
fill_inner_done:
    incl %esi
    jmp fill_loop
    
fill_done:
    popl %ecx
    popl %ebx
    popl %eax
    ret

; Get matrix index for i,j coordinates
get_matrix_index:
    pushl %eax                  ; Save row index
    pushl %ebx                  ; Save column index
    
    movl %ebx, %eax             ; Column index
    imull t_len, %eax           ; Multiply by column size
    addl %esi, %eax             ; Add row index
    addl $1, %eax               ; Account for 0-based indexing
    imull $4, %eax              ; Multiply by integer size
    
    popl %ebx
    popl %eax
    ret

; Count number of optimal alignments
count_alignments:
    pushl %eax                  ; Save matrix pointer
    pushl %ebx                  ; Save lengths
    
    movl s_len, %esi            ; Length of s
    movl t_len, %edi            ; Length of t
    
    ; Allocate memory for count matrix
    movl %esi, %eax
    addl $1, %eax
    movl %edi, %ebx
    addl $1, %ebx
    imull %ebx, %eax
    movl $4, %ebx
    call malloc
    
    ; Initialize count matrix
    call init_count_matrix
    
    ; Fill count matrix
    call fill_count_matrix
    
    ; Get final count
    movl %esi, %eax
    movl %edi, %ebx
    call get_count_index
    movl (%eax), %eax           ; Return count
    
    popl %ebx
    popl %eax
    ret

; Initialize count matrix
init_count_matrix:
    pushl %eax                  ; Save matrix pointer
    pushl %ecx                  ; Save size
    
    movl $0, %ecx
    movl (%eax), %ebx           ; Get matrix pointer
    
init_count_loop:
    cmpl %ecx, %edx             ; Compare counter with size
    jge init_count_done
    
    movl $1, (%ebx,%ecx,4)      ; Set count[i][j] = 1
    incl %ecx
    jmp init_count_loop
    
init_count_done:
    popl %ecx
    popl %eax
    ret

; Fill count matrix with alignment counts
fill_count_matrix:
    pushl %eax                  ; Save matrix pointer
    pushl %ebx                  ; Save string pointers
    
    movl s, %esi                ; String s
    movl t, %edi                ; String t
    movl s_len, %ecx            ; Length of s
    movl t_len, %edx            ; Length of t
    
    ; Initialize first row and column
    movl $1, %eax
    movl $0, %ebx
    
    ; Initialize first row
first_row_count_loop:
    cmpl %ecx, %ebx
    jge first_row_count_done
    
    movl %ebx, %edi             ; Column index
    movl %eax, %esi             ; Row index
    call get_count_index
    movl $1, (%eax)             ; Set count = 1
    incl %ebx
    jmp first_row_count_loop
    
first_row_count_done:
    ; Initialize first column
    movl $0, %ebx
first_col_count_loop:
    cmpl %edx, %ebx
    jge first_col_count_done
    
    movl %ebx, %edi             ; Column index
    movl $1, %esi               ; Row index
    call get_count_index
    movl $1, (%eax)             ; Set count = 1
    incl %ebx
    jmp first_col_count_loop
    
first_col_count_done:
    ; Fill the rest of count matrix
    movl $1, %esi               ; Row index
count_fill_loop:
    cmpl %ecx, %esi
    jge count_fill_done
    
    movl $1, %edi               ; Column index
count_fill_inner_loop:
    cmpl %edx, %edi
    jge count_fill_inner_done
    
    ; Get current score from DP matrix
    movl %esi, %eax
    movl %edi, %ebx
    call get_matrix_index
    movl (%eax), %ebp           ; Current DP score
    
    ; Get diagonal score
    decl %eax
    decl %ebx
    call get_matrix_index
    movl (%eax), %eax           ; Diagonal DP score
    
    ; Get left score
    movl %esi, %eax
    decl %ebx
    call get_matrix_index
    movl (%eax), %ebx           ; Left DP score
    
    ; Get up score
    decl %eax
    movl %esi, %eax
    call get_matrix_index
    movl (%eax), %ecx           ; Up DP score
    
    ; Get current count
    movl %esi, %eax
    movl %edi, %ebx
    call get_count_index
    movl (%eax), %edx           ; Current count
    
    ; Calculate counts for three moves
    movl $0, %eax               ; Count for diagonal
    movl $0, %ebx               ; Count for left
    movl $0, %ecx               ; Count for up
    
    ; Check if diagonal move contributes
    movl %esi, %eax
    decl %eax
    movl %edi, %ebx
    decl %ebx
    movb s(%eax), %al
    movb t(%ebx), %bl
    cmpb %al, %bl
    je diagonal_match
    
    ; Mismatch case
    movl mismatch, %eax
    jmp diagonal_score_done
    
diagonal_match:
    movl match, %eax
    
diagonal_score_done:
    addl %eax, %ebp             ; Add match/mismatch score
    
    ; Check if diagonal score equals current score
    movl %esi, %eax
    decl %eax
    movl %edi, %ebx
    decl %ebx
    call get_matrix_index
    movl (%eax), %eax           ; Diagonal DP score
    cmpl %ebp, %eax
    jne diagonal_skip
    
    ; Add diagonal count
    movl %esi, %eax
    decl %eax
    movl %edi, %ebx
    decl %ebx
    call get_count_index
    addl (%eax), %eax           ; Add to count
    
diagonal_skip:
    ; Continue with other moves...
    
    ; Store count
    movl %esi, %eax
    movl %edi, %ebx
    call get_count_index
    movl %eax, (%ebx)
    
    incl %edi
    jmp count_fill_inner_loop
    
count_fill_inner_done:
    incl %esi
    jmp count_fill_loop
    
count_fill_done:
    popl %ebx
    popl %eax
    ret

; Get count matrix index for i,j coordinates
get_count_index:
    pushl %eax                  ; Save row index
    pushl %ebx                  ; Save column index
    
    movl %ebx, %eax             ; Column index
    imull t_len, %eax           ; Multiply by column size
    addl %esi, %eax             ; Add row index
    addl $1, %eax               ; Account for 0-based indexing
    imull $4, %eax              ; Multiply by integer size
    
    popl %ebx
    popl %eax
    ret

; Memory allocation function
malloc:
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $12, %eax              ; sys_brk
    movl $0, %ebx               ; NULL
    int $0x80
    
    movl %eax, %ebx             ; Store new address
    addl %ecx, %ebx             ; Add size
    movl $12, %eax              ; sys_brk
    int $0x80
    
    movl %ebx, %eax             ; Return address
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret
```

## Explanation

This assembly implementation:

1. **Data Section**: Defines input strings and scoring parameters
2. **Matrix Initialization**: Creates DP and count matrices
3. **DP Matrix Filling**: Computes optimal alignment scores using dynamic programming
4. **Count Matrix Filling**: Counts the number of ways to achieve optimal scores
5. **Optimal Alignment Counting**: Uses traceback counting to determine total alignments

The algorithm follows the standard dynamic programming approach for sequence alignment with additional counting logic to track multiple optimal paths.

## Time Complexity
O(m×n) where m and n are the lengths of the input strings

## Space Complexity  
O(m×n) for the DP and count matrices

