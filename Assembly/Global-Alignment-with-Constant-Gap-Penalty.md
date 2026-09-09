# Rosalind Problem: Global Alignment with Constant Gap Penalty

I'll solve this step by step using assembly language (using x86-64 assembly syntax).

## Problem Understanding

We need to implement global sequence alignment with a constant gap penalty, similar to the Needleman-Wunsch algorithm.

## Solution

```assembly
.section .data
    # Sequence strings (example)
    seq1: .ascii "ACGTACGT"
    seq2: .ascii "ACGTACGT"
    len1: .long 8
    len2: .long 8
    
    # Scoring parameters
    match_score: .long 1
    mismatch_score: .long -1
    gap_penalty: .long -1

.section .text
    .global _start

# Function: global_alignment
# Input: 
#   rdi = seq1 pointer
#   rsi = seq2 pointer  
#   rdx = len1
#   rcx = len2
# Output:
#   rax = alignment score
global_alignment:
    # Allocate DP matrix (len1+1) x (len2+1)
    push rbp
    mov rbp, rsp
    
    # Calculate matrix dimensions
    mov r8, rdx          # len1
    mov r9, rcx          # len2
    inc r8               # len1 + 1
    inc r9               # len2 + 1
    
    # Allocate memory for DP matrix (4 bytes per cell)
    mov rax, r8
    mul r9               # size = (len1+1) * (len2+1) * 4
    mov r10, rax         # store size
    
    # Allocate memory
    mov rdi, rax
    call malloc
    
    # Initialize DP matrix
    mov r11, rax         # DP matrix base address
    
    # Initialize first row (gap penalties)
    xor rax, rax
    mov r12, 0           # i = 0
init_row_loop:
    cmp r12, r9
    jge init_row_done
    
    # Calculate position in matrix: [0][j] = j * gap_penalty
    mov r13, r12         # j
    imul r13, gap_penalty
    mov [r11 + r12*4], r13  # DP[0][j] = j * gap_penalty
    
    inc r12
    jmp init_row_loop
init_row_done:
    
    # Initialize first column (gap penalties)
    xor rax, rax
    mov r12, 1           # i = 1 (skip row 0)
init_col_loop:
    cmp r12, r8
    jge init_col_done
    
    # Calculate position in matrix: [i][0] = i * gap_penalty
    mov r13, r12         # i
    imul r13, gap_penalty
    mov [r11 + r12*r9*4], r13  # DP[i][0] = i * gap_penalty
    
    inc r12
    jmp init_col_loop
init_col_done:
    
    # Fill the DP matrix
    mov r12, 1           # i = 1
fill_matrix_loop:
    cmp r12, r8
    jge fill_done
    
    mov r13, 1           # j = 1
fill_inner_loop:
    cmp r13, r9
    jge fill_next_row
    
    # Calculate indices for current cell
    mov r14, r12         # i
    mov r15, r13         # j
    
    # Get scores from three possible sources:
    # 1. Diagonal (match/mismatch)
    mov eax, [r11 + (r14-1)*r9*4 + (r15-1)*4]  # DP[i-1][j-1]
    
    # Check if characters match
    mov al, [rdi + r14-1]      # seq1[i-1]
    mov bl, [rsi + r15-1]      # seq2[j-1]
    cmp al, bl
    je match_score_case
    # Mismatch case
    mov eax, [r11 + (r14-1)*r9*4 + (r15-1)*4]
    sub eax, mismatch_score
    jmp diagonal_done
match_score_case:
    # Match case
    mov eax, [r11 + (r14-1)*r9*4 + (r15-1)*4]
    add eax, match_score
    
diagonal_done:
    
    # 2. Left gap (from DP[i][j-1])
    mov ebx, [r11 + r14*r9*4 + (r15-1)*4]
    add ebx, gap_penalty
    
    # 3. Top gap (from DP[i-1][j])  
    mov ecx, [r11 + (r14-1)*r9*4 + r15*4]
    add ecx, gap_penalty
    
    # Find maximum of three scores
    mov edx, eax         # diagonal score
    cmp ebx, edx
    jle skip_left
    mov edx, ebx         # left score is max
skip_left:
    cmp ecx, edx
    jle skip_top
    mov edx, ecx         # top score is max
skip_top:
    
    # Store the maximum in DP[i][j]
    mov [r11 + r14*r9*4 + r15*4], edx
    
    inc r13
    jmp fill_inner_loop
    
fill_next_row:
    inc r12
    jmp fill_matrix_loop
fill_done:

    # Return the final score (bottom-right cell)
    mov eax, [r11 + (r8-1)*r9*4 + (r9-1)*4]
    
    # Clean up and return
    pop rbp
    ret

# Main function to demonstrate usage
_start:
    # Initialize parameters for example sequences
    mov rdi, seq1        # sequence 1 pointer
    mov rsi, seq2        # sequence 2 pointer
    mov rdx, len1        # length of seq1
    mov rcx, len2        # length of seq2
    
    call global_alignment
    
    # Exit program
    mov rax, 60          # sys_exit
    mov rdi, 0           # exit status
    syscall
```

## Algorithm Explanation

This assembly implementation follows the Needleman-Wunsch algorithm for global sequence alignment:

1. **Initialization**: 
   - Create a DP matrix of size (m+1) × (n+1)
   - Initialize first row and column with gap penalties
   - First row: [0][j] = j × gap_penalty
   - First column: [i][0] = i × gap_penalty

2. **Matrix Filling**:
   - For each cell [i][j], calculate three possible scores:
     - Diagonal: DP[i-1][j-1] + match/mismatch score
     - Left: DP[i][j-1] + gap penalty  
     - Top: DP[i-1][j] + gap penalty
   - Store the maximum of these three scores

3. **Result**: 
   - The bottom-right cell contains the optimal global alignment score

## Key Features

- Uses constant gap penalty for all gap operations
- Handles both match and mismatch scoring
- Memory efficient implementation using dynamic allocation
- Follows standard Needleman-Wunsch algorithm complexity O(mn)

## Usage

The function `global_alignment` takes four parameters:
- rdi: pointer to first sequence  
- rsi: pointer to second sequence
- rdx: length of first sequence
- rcx: length of second sequence

Returns the optimal alignment score in rax.

This implementation demonstrates how to solve the global alignment problem with constant gap penalty using assembly language concepts.