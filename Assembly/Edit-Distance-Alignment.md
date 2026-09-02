# Rosalind Problem: Edit Distance Alignment (Assembly)

I'll solve this step by step, implementing an edit distance alignment algorithm using Assembly language concepts.

## Problem Understanding

The Edit Distance Alignment problem requires finding the minimum number of operations (insertions, deletions, substitutions) to transform one string into another, along with the actual alignment.

## Solution Approach

I'll implement a dynamic programming solution with:
1. A scoring matrix for edit distance
2. Backtracking to reconstruct the alignment
3. Operations: insert, delete, substitute

## Assembly Implementation

```assembly
.section .data
    # Input strings
    str1:   .ascii "AGTACG"
    str2:   .ascii "ACGTACG"
    len1:   .long 6
    len2:   .long 7
    
    # Scoring parameters
    match_score:    .long 0
    mismatch_score: .long 1
    gap_penalty:    .long 1

.section .text
    .global _start

_start:
    # Load string lengths
    movl len1(%esp), %eax
    movl len2(%esp), %ebx
    
    # Calculate matrix dimensions (including 1 for padding)
    incl %eax
    incl %ebx
    
    # Allocate memory for DP matrix: (len1+1) x (len2+1)
    # Each cell is 4 bytes (32-bit integer)
    movl %eax, %ecx
    imull %ebx, %ecx
    movl %ecx, %edx
    shll $2, %edx          # Multiply by 4 for byte size
    
    # Allocate memory (simplified - in real assembly we'd use system calls)
    # For now, assume memory is allocated at address %esi
    
    # Initialize DP matrix
    call initialize_matrix
    
    # Fill the DP matrix
    call fill_dp_matrix
    
    # Trace back to get alignment
    call traceback_alignment
    
    # Exit program
    movl $1, %eax          # sys_exit
    movl $0, %ebx          # exit status
    int $0x80

initialize_matrix:
    # Initialize first row and column with gap penalties
    pushl %eax             # Save len1
    pushl %ebx             # Save len2
    
    # Initialize first row (0 to len2)
    movl $0, %ecx          # i = 0
    movl $0, %edi          # j = 0
    
init_row_loop:
    cmpl %ebx, %ecx        # Compare with len2
    jge init_row_done
    
    # Calculate matrix index: row * cols + col
    movl %ecx, %edx
    imull %eax, %edx       # edx = col * (len1+1)
    addl %ecx, %edx        # edx = col * (len1+1) + col = col * (len1+2)
    
    # Set matrix[0][col] = col * gap_penalty
    movl %ecx, %esi        # col
    imull gap_penalty, %esi
    movl %esi, (%edx)      # matrix[0][col] = col * gap_penalty
    
    incl %ecx              # col++
    jmp init_row_loop
    
init_row_done:
    # Initialize first column (0 to len1)
    movl $0, %ecx          # i = 0
init_col_loop:
    cmpl %eax, %ecx        # Compare with len1
    jge init_col_done
    
    # Calculate matrix index: row * cols + col
    movl %ecx, %edx
    imull %ebx, %edx       # edx = row * (len2+1)
    
    # Set matrix[row][0] = row * gap_penalty
    movl %ecx, %esi        # row
    imull gap_penalty, %esi
    movl %esi, (%edx)      # matrix[row][0] = row * gap_penalty
    
    incl %ecx              # row++
    jmp init_col_loop
    
init_col_done:
    popl %ebx              # Restore len2
    popl %eax              # Restore len1
    ret

fill_dp_matrix:
    pushl %eax             # Save len1
    pushl %ebx             # Save len2
    
    # i from 1 to len1
    movl $1, %ecx          # i = 1
fill_i_loop:
    cmpl %eax, %ecx        # Compare with len1
    jge fill_i_done
    
    # j from 1 to len2
    movl $1, %edi          # j = 1
fill_j_loop:
    cmpl %ebx, %edi        # Compare with len2
    jge fill_j_done
    
    # Calculate matrix indices for current position (i,j)
    movl %ecx, %edx        # i
    imull %ebx, %edx       # i * (len2+1)
    addl %edi, %edx        # i * (len2+1) + j
    
    # Get characters from strings
    movb str1-1(%ecx), %al # str1[i-1] (adjust for 0-based indexing)
    movb str2-1(%edi), %bl # str2[j-1]
    
    # Calculate substitution cost
    movl $0, %esi          # cost = 0
    cmpl %al, %bl          # compare characters
    je match_case          # if equal, cost = 0
    
    # Mismatch case - add mismatch penalty
    movl mismatch_score, %esi
    jmp calc_min
    
match_case:
    movl match_score, %esi # cost = 0 for match
    
calc_min:
    # Calculate three possible operations:
    # 1. Deletion: matrix[i-1][j] + gap_penalty
    movl %ecx, %edx        # i
    decl %edx              # i-1
    imull %ebx, %edx       # (i-1) * (len2+1)
    addl %edi, %edx        # + j
    movl (%edx), %edi      # matrix[i-1][j]
    addl gap_penalty, %edi # + gap_penalty
    
    # 2. Insertion: matrix[i][j-1] + gap_penalty  
    movl %ecx, %edx        # i
    imull %ebx, %edx       # i * (len2+1)
    addl %edi, %edx        # i * (len2+1) + (j-1)
    movl (%edx), %edx      # matrix[i][j-1]
    addl gap_penalty, %edx # + gap_penalty
    
    # 3. Substitution: matrix[i-1][j-1] + cost
    movl %ecx, %edx        # i
    decl %edx              # i-1
    imull %ebx, %edx       # (i-1) * (len2+1)
    addl %edi, %edx        # + (j-1)
    movl (%edx), %edx      # matrix[i-1][j-1]
    addl %esi, %edx        # + cost
    
    # Find minimum of three values
    movl %edi, %eax        # deletion value
    cmpl %edx, %eax        # compare with insertion
    jge use_insertion      # if deletion >= insertion
    movl %edx, %eax        # use insertion
    
use_insertion:
    cmpl %esi, %eax        # compare with substitution
    jge store_result       # if insertion >= substitution
    movl %esi, %eax        # use substitution
    
store_result:
    # Store result in matrix[i][j]
    movl %ecx, %edx        # i
    imull %ebx, %edx       # i * (len2+1)
    addl %edi, %edx        # + j
    movl %eax, (%edx)      # matrix[i][j] = min_value
    
fill_j_done:
    incl %edi              # j++
    jmp fill_j_loop
    
fill_i_done:
    popl %ebx              # Restore len2
    popl %eax              # Restore len1
    ret

traceback_alignment:
    # Start from bottom-right corner (len1, len2)
    movl len1, %ecx        # i = len1
    movl len2, %edi        # j = len2
    
    # Allocate space for alignment strings
    # This is a simplified version - in practice we'd need to manage memory
    
traceback_loop:
    cmpl $0, %ecx          # i == 0?
    je traceback_done      # if yes, done
    cmpl $0, %edi          # j == 0?
    je traceback_done      # if yes, done
    
    # Calculate current matrix position
    movl %ecx, %edx        # i
    imull len2, %edx       # i * (len2+1)
    addl %edi, %edx        # + j
    
    # Get current score from matrix
    movl (%edx), %esi      # current score
    
    # Calculate three possible previous positions:
    # 1. Diagonal (substitution): matrix[i-1][j-1]
    movl %ecx, %eax        # i
    decl %eax              # i-1
    imull len2, %eax       # (i-1) * (len2+1)
    addl %edi, %eax        # + (j-1)
    movl (%eax), %eax      # matrix[i-1][j-1]
    
    # 2. Up (deletion): matrix[i-1][j]  
    movl %ecx, %eax        # i
    decl %eax              # i-1
    imull len2, %eax       # (i-1) * (len2+1)
    addl %edi, %eax        # + j
    movl (%eax), %eax      # matrix[i-1][j]
    
    # 3. Left (insertion): matrix[i][j-1]
    movl %ecx, %eax        # i
    imull len2, %eax       # i * (len2+1)
    addl %edi, %eax        # + (j-1)
    movl (%eax), %eax      # matrix[i][j-1]
    
    # Compare to determine operation type
    # Check if we came from diagonal (substitution or match)
    addl $1, %eax          # Add gap penalty to previous score
    cmpl %esi, %eax        # compare with current score
    je diagonal_move       # if equal, came from diagonal
    
    # Check if we came from up (deletion)
    addl gap_penalty, %eax # Add gap penalty to previous score  
    cmpl %esi, %eax        # compare with current score
    je up_move             # if equal, came from up
    
    # Otherwise came from left (insertion)
    addl gap_penalty, %eax # Add gap penalty to previous score
    cmpl %esi, %eax        # compare with current score
    je left_move           # if equal, came from left
    
diagonal_move:
    # Move diagonally and add substitution/match to alignment
    decl %ecx              # i--
    decl %edi              # j--
    jmp traceback_loop
    
up_move:
    # Move up (deletion)
    decl %ecx              # i--
    jmp traceback_loop
    
left_move:
    # Move left (insertion)  
    decl %edi              # j--
    jmp traceback_loop
    
traceback_done:
    ret

# Helper functions for string operations would go here
```

## Explanation of Key Components

### 1. **Memory Management**
- Allocate space for DP matrix of size `(len1+1) × (len2+1)`
- Each cell stores integer edit distance values

### 2. **Matrix Initialization**
- First row: `0, gap_penalty, 2×gap_penalty, ...`
- First column: `0, gap_penalty, 2×gap_penalty, ...`

### 3. **Dynamic Programming Fill**
```
matrix[i][j] = min(
    matrix[i-1][j] + gap_penalty,     // deletion
    matrix[i][j-1] + gap_penalty,     // insertion  
    matrix[i-1][j-1] + cost           // substitution/match
)
```

### 4. **Backtracking**
- Start from bottom-right corner
- Trace back using the minimum operation path
- Build alignment strings by following optimal moves

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are string lengths
- **Space Complexity**: O(m×n) for the DP matrix

This implementation provides the foundation for solving edit distance alignment problems in assembly language, though a full working version would require proper system call handling and memory management.