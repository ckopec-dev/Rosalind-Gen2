# Rosalind Problem: Overlap Alignment

## Problem Statement
Given two strings s and t, find an alignment that maximizes the overlap between them.

## Solution in Assembly

```assembly
; Overlap Alignment implementation
; Input: Two strings s and t
; Output: Maximum overlap score and alignment

.data
    s:      .asciiz "AGTACG"
    t:      .asciiz "ACGTACG"
    m:      .word 7          ; length of s
    n:      .word 7          ; length of t
    match:  .word 2          ; match score
    mismatch:.word -1        ; mismatch score
    gap:    .word -1         ; gap penalty

.text
.globl _start

_start:
    ; Load string lengths
    lw $t0, m              ; m = length of s
    lw $t1, n              ; n = length of t
    
    ; Allocate memory for DP table (m+1) x (n+1)
    addi $t2, $t0, 1       ; m + 1
    addi $t3, $t1, 1       ; n + 1
    mul $t4, $t2, $t3      ; size = (m+1) * (n+1)
    li $v0, 9              ; sbrk system call
    mul $a0, $t4, 4        ; multiply by 4 (word size)
    syscall
    
    ; Store DP table base address
    move $s0, $v0          ; $s0 = DP table base address
    
    ; Initialize first row to zero (overlap alignment)
    li $t5, 0              ; i = 0
outer_loop:
    beq $t5, $t2, end_init ; if i == m+1, done
    li $t6, 0              ; j = 0
inner_loop:
    beq $t6, $t3, next_row ; if j == n+1, next row
    
    ; Calculate table position: DP[i][j] = i*(n+1) + j
    mul $t7, $t5, $t3      ; i * (n+1)
    add $t7, $t7, $t6      ; + j
    mul $t7, $t7, 4        ; multiply by word size
    
    ; Initialize first row to zero (overlap alignment)
    sw $zero, 0($s0)       ; DP[0][j] = 0
    
    addi $t6, $t6, 1       ; j++
    j inner_loop
next_row:
    addi $t5, $t5, 1       ; i++
    j outer_loop
end_init:

    ; Fill the DP table
    li $t5, 1              ; i = 1
fill_outer:
    beq $t5, $t2, find_max ; if i == m+1, done filling
    
    li $t6, 1              ; j = 1
fill_inner:
    beq $t6, $t3, fill_next ; if j == n+1, next row
    
    ; Calculate table positions
    mul $t7, $t5, $t3      ; i * (n+1)
    add $t7, $t7, $t6      ; + j
    mul $t7, $t7, 4        ; multiply by word size
    
    ; Get characters from strings
    la $a0, s              ; load s address
    la $a1, t              ; load t address
    add $a2, $a0, $t5      ; s[i-1]
    add $a3, $a1, $t6      ; t[j-1]
    
    lb $t8, 0($a2)         ; s[i-1] character
    lb $t9, 0($a3)         ; t[j-1] character
    
    ; Calculate match/mismatch score
    beq $t8, $t9, match_case ; if characters match
    ; Mismatch case
    lw $t10, mismatch      ; load mismatch score
    j calculate_dp
    
match_case:
    ; Match case
    lw $t10, match         ; load match score
    
calculate_dp:
    ; Get DP[i-1][j-1] + score
    addi $t11, $t5, -1     ; i-1
    addi $t12, $t6, -1     ; j-1
    mul $t13, $t11, $t3    ; (i-1) * (n+1)
    add $t13, $t13, $t12   ; + (j-1)
    mul $t13, $t13, 4      ; multiply by word size
    lw $t14, 0($s0)        ; DP[i-1][j-1]
    add $t14, $t14, $t10   ; + score
    
    ; Get DP[i-1][j] - gap penalty
    addi $t15, $t5, -1     ; i-1
    mul $t16, $t15, $t3    ; (i-1) * (n+1)
    add $t16, $t16, $t6    ; + j
    mul $t16, $t16, 4      ; multiply by word size
    lw $t17, 0($s0)        ; DP[i-1][j]
    lw $t18, gap           ; load gap penalty
    sub $t17, $t17, $t18   ; - gap penalty
    
    ; Get DP[i][j-1] - gap penalty
    addi $t19, $t6, -1     ; j-1
    mul $t20, $t5, $t3     ; i * (n+1)
    add $t20, $t20, $t19   ; + (j-1)
    mul $t20, $t20, 4      ; multiply by word size
    lw $t21, 0($s0)        ; DP[i][j-1]
    sub $t21, $t21, $t18   ; - gap penalty
    
    ; Find maximum of three values
    li $t22, 0             ; max = 0
    
    bge $t14, $t22, check_1
    move $t22, $t14        ; max = DP[i-1][j-1] + score
    
check_1:
    bge $t17, $t22, check_2
    move $t22, $t17        ; max = DP[i-1][j] - gap
    
check_2:
    bge $t21, $t22, store_result
    move $t22, $t21        ; max = DP[i][j-1] - gap
    
store_result:
    sw $t22, 0($s0)        ; store result in DP[i][j]
    
    addi $t6, $t6, 1       ; j++
    j fill_inner
    
fill_next:
    addi $t5, $t5, 1       ; i++
    j fill_outer
    
find_max:
    ; Find maximum value in last row
    li $t5, 0              ; i = 0 (start with first element)
    lw $t6, 0($s0)         ; max = DP[0][0]
    
    li $t7, 1              ; j = 1
max_loop:
    beq $t7, $t3, print_result ; if j == n+1, done
    
    ; Calculate position in table
    mul $t8, $t2, $t3      ; m+1 * (n+1)
    add $t8, $t8, $t7      ; + j
    mul $t8, $t8, 4        ; multiply by word size
    
    lw $t9, 0($s0)         ; get DP[m][j]
    bge $t9, $t6, max_update
    move $t6, $t9          ; update max
    
max_update:
    addi $t7, $t7, 1       ; j++
    j max_loop
    
print_result:
    ; Print maximum overlap score
    li $v0, 1              ; print integer system call
    move $a0, $t6          ; load max value
    syscall
    
    ; Exit program
    li $v0, 10             ; exit system call
    syscall
```

## Explanation

This assembly implementation solves the overlap alignment problem by:

1. **Memory Allocation**: Creates a DP table of size (m+1) × (n+1) where m and n are the lengths of the input strings.

2. **Initialization**: 
   - Sets the first row to zero values (overlap alignment constraint)
   - Each cell represents the maximum overlap score ending at that position

3. **DP Fill Process**:
   - For each cell DP[i][j], calculates three possible scores:
     - Match/mismatch with diagonal element + scoring
     - Gap penalty from top cell
     - Gap penalty from left cell
   - Takes the maximum of these three values

4. **Result Extraction**: 
   - Finds the maximum value in the last row (since we want overlap alignment)
   - This represents the best overlap score between the two strings

The algorithm uses dynamic programming with time complexity O(m×n) and space complexity O(m×n).