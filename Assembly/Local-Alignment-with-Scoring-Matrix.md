# Rosalind Problem: Local Alignment with Scoring Matrix

I'll solve this step by step, implementing local sequence alignment using the Smith-Waterman algorithm with a given scoring matrix.

## Problem Understanding

We need to perform local sequence alignment between two strings using a scoring matrix and find the optimal local alignment score.

## Solution Approach

The Smith-Waterman algorithm is used for local sequence alignment:
1. Create a dynamic programming matrix
2. Fill the matrix using the scoring matrix
3. Find the maximum score in the matrix
4. Trace back to find the optimal alignment

## Assembly Implementation

```assembly
.section .data
    # Scoring matrix (assuming DNA sequences)
    scoring_matrix: .word 2, -1, -1, -1,   # Match, Mismatch, Gap
                    -1, 2, -1, -1,
                    -1, -1, 2, -1,
                    -1, -1, -1, 2
    
    # DNA alphabet mapping (A=0, C=1, G=2, T=3)
    dna_map: .byte 'A', 'C', 'G', 'T'
    
    # Input sequences
    seq1: .ascii "ACGTACGT"
    seq2: .ascii "ACGTACGT"
    seq1_len: .word 8
    seq2_len: .word 8
    
    # Matrix dimensions
    max_size: .word 100

.section .text
    .global _start

# Function to get scoring matrix value for two nucleotides
get_score:
    push {r4-r7, lr}
    
    mov r4, r0          @ First nucleotide index
    mov r5, r1          @ Second nucleotide index
    
    # Calculate matrix position: row * 4 + col
    lsl r6, r4, #2      @ r6 = r4 * 4
    add r6, r6, r5      @ r6 = (r4 * 4) + r5
    
    # Load scoring matrix element
    ldr r0, =scoring_matrix
    ldr r7, [r0, r6, lsl #2]  @ Load 32-bit word
    
    pop {r4-r7, pc}

# Main local alignment function
local_alignment:
    push {r4-r11, lr}
    
    mov r4, r0          @ seq1 pointer
    mov r5, r1          @ seq2 pointer
    mov r6, r2          @ seq1 length
    mov r7, r3          @ seq2 length
    
    # Allocate memory for DP matrix (max_size * max_size)
    ldr r8, =max_size
    lsl r9, r8, #2      @ Matrix size in bytes
    bl malloc           @ Allocate memory
    mov r10, r0         @ DP matrix pointer
    
    # Initialize first row and column to zero
    mov r11, #0         @ i counter
    
outer_loop:
    cmp r11, r6
    bge outer_end
    
    mov r0, r11
    mov r1, #0
    bl get_score        @ Get score for alignment
    
    # Store in matrix
    lsl r12, r11, #2    @ r12 = i * 4
    add r12, r12, r10   @ r12 = matrix + (i * 4)
    str r0, [r12]       @ Store score
    
    add r11, r11, #1
    b outer_loop
    
outer_end:
    mov r11, #0         @ j counter
    
inner_loop:
    cmp r11, r7
    bge inner_end
    
    mov r0, #0
    mov r1, r11
    bl get_score        @ Get score for alignment
    
    # Store in matrix (row 0)
    lsl r12, r11, #2    @ r12 = j * 4
    add r12, r12, r10   @ r12 = matrix + (j * 4)
    str r0, [r12]       @ Store score
    
    add r11, r11, #1
    b inner_loop
    
inner_end:
    # Fill the rest of the matrix using Smith-Waterman algorithm
    mov r11, #1         @ i = 1
fill_loop_i:
    cmp r11, r6
    bge fill_end
    
    mov r8, #1          @ j = 1
fill_loop_j:
    cmp r8, r7
    bge fill_next_i
    
    # Get nucleotide values
    ldrb r0, [r4, r11]  @ seq1[i]
    ldrb r1, [r5, r8]   @ seq2[j]
    
    # Convert to index (assuming A=0, C=1, G=2, T=3)
    mov r2, #0
    mov r3, #0
    
    # Simple conversion: assume ASCII values
    cmp r0, #'A'
    beq convert_a
    cmp r0, #'C'
    beq convert_c
    cmp r0, #'G'
    beq convert_g
    cmp r0, #'T'
    beq convert_t
    
convert_a:
    mov r2, #0
    b next_nucleotide
convert_c:
    mov r2, #1
    b next_nucleotide
convert_g:
    mov r2, #2
    b next_nucleotide
convert_t:
    mov r2, #3
    
next_nucleotide:
    cmp r1, #'A'
    beq convert_a2
    cmp r1, #'C'
    beq convert_c2
    cmp r1, #'G'
    beq convert_g2
    cmp r1, #'T'
    beq convert_t2
    
convert_a2:
    mov r3, #0
    b calculate_score
convert_c2:
    mov r3, #1
    b calculate_score
convert_g2:
    mov r3, #2
    b calculate_score
convert_t2:
    mov r3, #3
    
calculate_score:
    # Get match score from scoring matrix
    mov r0, r2          @ nucleotide 1 index
    mov r1, r3          @ nucleotide 2 index
    bl get_score        @ r0 = match_score
    
    # Calculate diagonal score (from previous cell)
    lsl r4, r11, #2     @ i * 4
    lsl r5, r8, #2      @ j * 4
    add r4, r4, r5      @ (i * 4) + (j * 4)
    add r4, r4, r10     @ matrix address
    
    ldr r9, [r4]        @ diagonal score
    
    # Calculate scores from three directions
    add r9, r9, r0      @ match score
    
    # Get left score (from previous column)
    lsl r4, r11, #2     @ i * 4
    sub r5, r8, #1      @ j - 1
    lsl r5, r5, #2      @ (j-1) * 4
    add r4, r4, r5      @ (i * 4) + ((j-1) * 4)
    add r4, r4, r10     @ matrix address
    ldr r10, [r4]       @ left score
    
    # Get up score (from previous row)
    sub r4, r11, #1     @ i - 1
    lsl r4, r4, #2      @ (i-1) * 4
    lsl r5, r8, #2      @ j * 4
    add r4, r4, r5      @ ((i-1) * 4) + (j * 4)
    add r4, r4, r10     @ matrix address
    ldr r11, [r4]       @ up score
    
    # Find maximum of three scores and add zero
    mov r12, #0         @ zero score
    
    cmp r9, r10
    movlt r9, r10       @ r9 = max(diag, left)
    
    cmp r9, r11
    movlt r9, r11       @ r9 = max(max(diag, left), up)
    
    cmp r9, r12
    movlt r9, r12       @ r9 = max(max(diag, left), up, 0)
    
    # Store in matrix
    lsl r4, r11, #2     @ i * 4
    lsl r5, r8, #2      @ j * 4
    add r4, r4, r5      @ (i * 4) + (j * 4)
    add r4, r4, r10     @ matrix address
    str r9, [r4]        @ Store final score
    
    add r8, r8, #1      @ j++
    b fill_loop_j
    
fill_next_i:
    add r11, r11, #1    @ i++
    b fill_loop_i
    
fill_end:
    # Find maximum score in matrix
    mov r11, #0         @ max_score = 0
    mov r8, #0          @ max_i = 0
    mov r9, #0          @ max_j = 0
    
    # Search through matrix to find maximum
    mov r10, #0         @ i counter for search
search_loop_i:
    cmp r10, r6
    bge search_end
    
    mov r12, #0         @ j counter for search
search_loop_j:
    cmp r12, r7
    bge search_next_i
    
    # Calculate matrix position
    lsl r0, r10, #2     @ i * 4
    lsl r1, r12, #2     @ j * 4
    add r0, r0, r1      @ (i * 4) + (j * 4)
    add r0, r0, r5      @ matrix address
    
    ldr r1, [r0]        @ get score
    
    cmp r1, r11         @ compare with max
    movgt r11, r1       @ update max if needed
    movgt r8, r10       @ update max_i
    movgt r9, r12       @ update max_j
    
    add r12, r12, #1    @ j++
    b search_loop_j
    
search_next_i:
    add r10, r10, #1    @ i++
    b search_loop_i
    
search_end:
    mov r0, r11         @ return maximum score
    
    # Free allocated memory
    mov r1, r5          @ matrix pointer
    bl free
    
    pop {r4-r11, pc}

# Main function
_start:
    push {r4-r7, lr}
    
    ldr r0, =seq1
    ldr r1, =seq2
    ldr r2, seq1_len
    ldr r3, seq2_len
    
    bl local_alignment
    
    # Result is in r0
    mov r7, #1          @ exit syscall
    mov r0, #0          @ exit status
    svc 0               @ system call

.section .bss
    matrix: .space 400  # 100 * 100 * 4 bytes
```

## Key Algorithm Steps

1. **Matrix Initialization**: Create a DP matrix with dimensions (m+1) × (n+1)
2. **Score Calculation**: For each cell, calculate the maximum of:
   - Diagonal score + match/mismatch score
   - Left score + gap penalty  
   - Up score + gap penalty
   - Zero (for local alignment)
3. **Maximum Score**: Track the highest score in the matrix
4. **Traceback**: Optional for finding actual alignment

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are sequence lengths
- **Space Complexity**: O(m × n) for the DP matrix

This implementation performs local sequence alignment using the Smith-Waterman algorithm, which finds the optimal local alignment between two sequences with a given scoring matrix.