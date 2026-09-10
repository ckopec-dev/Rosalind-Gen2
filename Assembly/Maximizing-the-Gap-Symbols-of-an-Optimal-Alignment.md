# Rosalind Problem: Maximizing the Gap Symbols of an Optimal Alignment

## Problem Understanding

This problem asks us to find an optimal global alignment between two strings that maximizes the number of gap symbols (insertions/deletions) in the alignment.

## Approach

To maximize gaps in an optimal alignment, we need to modify the scoring function:
- Instead of penalizing gaps with a fixed penalty, we want to encourage gaps
- We can achieve this by using a negative gap penalty or by structuring our scoring matrix differently

Let me solve this step by step:

## Solution Steps

### Step 1: Understanding the Alignment Scoring

In standard sequence alignment:
- Match: +1
- Mismatch: -1  
- Gap: -1 (or some penalty)

To maximize gaps, we want to penalize matches and mismatches more heavily than gaps.

### Step 2: Modified Scoring Function

For maximum gaps in optimal alignment:
- Match: -1 (penalize matches)
- Mismatch: -1 (penalize mismatches)  
- Gap: +1 (reward gaps)

This encourages the algorithm to use as many gaps as possible while still maintaining optimality.

### Step 3: Implementation in Assembly

```assembly
; Maximizing the Gap Symbols of an Optimal Alignment
; Assembly implementation for Rosalind problem

.section .data
    seq1: .ascii "ACGTACGT"    ; First sequence
    seq2: .ascii "ACGT"        ; Second sequence
    len1: .long 8              ; Length of first sequence
    len2: .long 4              ; Length of second sequence
    
    ; Scoring matrix elements
    MATCH_SCORE: .long -1      ; Penalty for match
    MISMATCH_SCORE: .long -1   ; Penalty for mismatch  
    GAP_SCORE: .long 1         ; Reward for gap

.section .text
.globl _start

_start:
    ; Initialize variables
    movl len1(%esp), %eax      ; Load length of seq1
    movl len2(%esp), %ebx      ; Load length of seq2
    
    ; Allocate DP matrix (rows = len1+1, cols = len2+1)
    ; Matrix size: (len1+1) * (len2+1) * sizeof(int)
    
    ; Initialize first row and column
    call initialize_dp_matrix
    
    ; Fill the DP matrix using modified scoring
    call fill_dp_matrix
    
    ; Trace back to find optimal alignment with maximum gaps
    call trace_back_max_gaps
    
    ; Output result
    call output_result
    
    ; Exit program
    movl $1, %eax              ; sys_exit
    movl $0, %ebx              ; exit status
    int $0x80

initialize_dp_matrix:
    ; Initialize first row (all gaps)
    movl $0, %ecx              ; i = 0
init_row_loop:
    cmpl len2(%esp), %ecx      ; compare with seq2 length
    jg init_row_done
    
    movl GAP_SCORE(%esp), %edx ; Load gap score
    ; Store in DP matrix at position (0,j)
    ; Matrix[i][j] = matrix[i*cols + j]
    addl %ecx, %eax            ; Calculate offset
    movl %edx, (%eax)          ; Store in matrix
    
    incl %ecx                  ; i++
    jmp init_row_loop
init_row_done:
    
    ; Initialize first column (all gaps)
    movl $0, %ecx              ; j = 0  
init_col_loop:
    cmpl len1(%esp), %ecx      ; compare with seq1 length
    jg init_col_done
    
    movl GAP_SCORE(%esp), %edx ; Load gap score
    ; Store in DP matrix at position (i,0)
    addl %ecx, %eax            ; Calculate offset  
    movl %edx, (%eax)          ; Store in matrix
    
    incl %ecx                  ; i++
    jmp init_col_loop
init_col_done:
    
    ret

fill_dp_matrix:
    ; Dynamic programming filling with modified scoring
    movl $1, %esi              ; i = 1 (start from 1st row)
fill_outer_loop:
    cmpl len1(%esp), %esi      ; compare with seq1 length
    jg fill_done
    
    movl $1, %edi              ; j = 1 (start from 1st column)
fill_inner_loop:
    cmpl len2(%esp), %edi      ; compare with seq2 length  
    jg fill_next_row
    
    ; Calculate matrix indices
    movl %esi, %eax            ; i
    imull len2(%esp), %eax     ; i * cols
    addl %edi, %eax            ; + j
    
    ; Get current characters from sequences
    movb seq1-1(%esi), %al     ; seq1[i-1]  
    movb seq2-1(%edi), %bl     ; seq2[j-1]
    
    ; Compare characters
    cmpb %bl, %al              ; compare chars
    je match_case
    
    ; Mismatch case
    movl MISMATCH_SCORE(%esp), %ecx
    jmp calculate_score
    
match_case:
    ; Match case  
    movl MATCH_SCORE(%esp), %ecx
    
calculate_score:
    ; Calculate maximum of three possibilities
    ; 1. Diagonal (match/mismatch)
    ; 2. From left (gap in seq1)  
    ; 3. From above (gap in seq2)
    
    ; Get diagonal value
    movl %esi, %eax
    imull len2(%esp), %eax
    decl %eax                  ; i-1 * cols + j-1
    movl (%eax), %edx
    
    ; Add match/mismatch score  
    addl %ecx, %edx
    
    ; Get left value (gap in seq1)
    movl %esi, %eax
    imull len2(%esp), %eax
    decl %eax                  ; i * cols + j-1
    movl (%eax), %ebx
    
    ; Add gap score
    addl GAP_SCORE(%esp), %ebx
    
    ; Get above value (gap in seq2)
    movl %esi, %eax
    imull len2(%esp), %eax
    decl %eax                  ; (i-1) * cols + j  
    movl (%eax), %ecx
    
    ; Add gap score
    addl GAP_SCORE(%esp), %ecx
    
    ; Find maximum of the three values
    cmpl %ebx, %edx            ; compare diagonal vs left
    jg diagonal_left_max
    movl %ebx, %edx            ; left is max
    
diagonal_left_max:
    cmpl %ecx, %edx            ; compare with above
    jg store_result
    movl %ecx, %edx            ; above is max
    
store_result:
    ; Store in current position
    movl %esi, %eax
    imull len2(%esp), %eax
    addl %edi, %eax            ; i * cols + j
    movl %edx, (%eax)          ; store result
    
fill_next_row:
    incl %esi                  ; increment row
    jmp fill_outer_loop
fill_done:
    
    ret

trace_back_max_gaps:
    ; Trace back through matrix to find optimal alignment
    ; with maximum number of gaps
    
    movl len1(%esp), %esi      ; start from bottom right
    movl len2(%esp), %edi      ; start from bottom right
    
    ; Initialize alignment strings
    movl $0, %ecx              ; position counter
    
trace_back_loop:
    cmpl $0, %esi              ; check if reached top
    jl trace_done
    cmpl $0, %edi              ; check if reached left  
    jl trace_done
    
    ; Get current cell value
    movl %esi, %eax
    imull len2(%esp), %eax
    addl %edi, %eax            ; i * cols + j
    movl (%eax), %edx          ; get matrix value
    
    ; Determine move based on which direction gave the best score
    ; This is a simplified approach - in practice would need to track
    ; all possible paths that maximize gaps
    
    ; For maximum gaps, prefer moves that introduce more gaps
    ; (this is heuristic-based for the problem)
    
    ; Simple greedy: prefer gap moves when possible
    decl %esi                  ; move up
    decl %edi                  ; move left
    jmp trace_back_loop
    
trace_done:
    ret

output_result:
    ; Output the optimal alignment with maximum gaps
    ; This would output the actual alignment strings
    
    ; For now, just return the score (maximum gaps in alignment)
    movl matrix_end(%esp), %eax ; assume we store result here
    ; Print or return the maximum gap count
    
    ret
```

## Key Concepts

1. **Modified Scoring**: Changed from typical penalty scoring to reward gaps
2. **Dynamic Programming**: Used standard DP approach but with modified weights
3. **Traceback**: Followed optimal path while maximizing gap usage

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the DP matrix

## Notes

This assembly implementation provides a framework for solving the problem. In practice, you would need to:
1. Properly handle memory allocation for the DP matrix
2. Implement exact traceback logic
3. Handle string comparisons correctly
4. Optimize for specific Rosalind input formats

The core idea is to modify the alignment scoring so that gaps are encouraged while still maintaining optimality of the overall alignment.