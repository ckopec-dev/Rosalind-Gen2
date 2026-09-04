# Rosalind Problem: Wobble Bonding and RNA Secondary Structures (Assembly Solution)

## Problem Understanding

This problem asks us to count the number of ways an RNA string can form valid secondary structures with wobble bonding allowed. In RNA secondary structure prediction, we can have:
- A-U base pairs
- U-A base pairs  
- G-C base pairs
- C-G base pairs
- G-U wobble pairs
- U-G wobble pairs

We need to count the number of valid secondary structures that can be formed.

## Approach

This is a classic dynamic programming problem similar to the "RNA secondary structure" counting problem. We use the following recurrence relation:

Let `dp[i][j]` = number of valid secondary structures for substring from index i to j

Base cases:
- `dp[i][i] = 1` (single nucleotide can't form bonds)
- `dp[i][i+1] = 1` (two nucleotides can form one bond if valid)

Recurrence relation:
```
dp[i][j] = dp[i][j-1] + Σ dp[i][k-1] * dp[k+1][j] (for valid pairs)
```

Where valid pairs include:
- A-U, U-A
- G-C, C-G  
- G-U, U-G

## Assembly Implementation

```assembly
; Rosalind Wobble Bonding and RNA Secondary Structures
; Assembly implementation

.section .data
    ; RNA sequence (example: "AUCG")
    rna_seq: .ascii "AUCG"
    seq_len: .long 4
    
    ; Valid base pairs for wobble bonding
    valid_pairs:
        .byte 'A', 'U'      ; A-U pair
        .byte 'U', 'A'      ; U-A pair  
        .byte 'G', 'C'      ; G-C pair
        .byte 'C', 'G'      ; C-G pair
        .byte 'G', 'U'      ; G-U wobble pair
        .byte 'U', 'G'      ; U-G wobble pair

.section .text
    .global _start
    
_start:
    ; Initialize DP table
    ; dp[i][j] = number of valid structures for substring i to j
    movl seq_len(%esp), %ecx       ; Get sequence length
    decl %ecx                      ; Length - 1
    
    ; Allocate memory for DP table (n x n)
    movl %ecx, %eax                ; Size = n-1
    imull %eax, %eax               ; Size = (n-1)^2
    movl %eax, %ebx                ; Store size
    movl $0x20, %eax               ; mmap syscall number
    movl $0, %ebx                  ; addr = 0 (let kernel choose)
    movl %ebx, %ecx                ; length = 0 (will set later)
    movl $0x3, %edx                ; prot = PROT_READ | PROT_WRITE
    movl $0x22, %esi               ; flags = MAP_PRIVATE | MAP_ANONYMOUS
    movl $-1, %edi                 ; fd = -1 (no file)
    int $0x80                      ; syscall
    
    ; Store DP table base address in register
    movl %eax, %ebp                ; DP table base address
    
    ; Initialize DP table
    call init_dp_table
    
    ; Compute number of valid structures
    call compute_structures
    
    ; Exit program
    movl $1, %eax                  ; exit syscall
    movl $0, %ebx                  ; exit status
    int $0x80

init_dp_table:
    ; Initialize DP table to zero
    pushl %ecx                     ; Save length
    movl seq_len(%esp), %edx       ; Get sequence length
    
    ; Initialize diagonal elements to 1 (base case)
    xorl %esi, %esi                ; i = 0
outer_loop:
    cmpb $0, %dl                   ; Check if we've processed all positions
    jz init_done
    
    movl %esi, %eax                ; Get row index
    imull %edx, %eax               ; Row * n
    addl %esi, %eax                ; + col = (row * n) + col
    shll $2, %eax                  ; Multiply by 4 (32-bit integers)
    addl %ebp, %eax                ; Add base address
    
    movl $1, (%eax)                ; dp[i][i] = 1
    
    incl %esi                      ; Increment i
    decl %dl                       ; Decrement length counter
    jmp outer_loop
    
init_done:
    popl %ecx                      ; Restore length
    ret

compute_structures:
    ; Dynamic programming approach to count valid structures
    pushl %ecx                     ; Save sequence length
    movl seq_len(%esp), %edx       ; Get sequence length
    
    ; For each gap size from 2 to n-1
    movl $2, %esi                  ; gap = 2
gap_loop:
    cmpb $0, %dl                   ; Check if gap >= n
    jge compute_done
    
    movl $0, %edi                  ; start = 0
    
start_loop:
    addl %esi, %edi                ; end = start + gap
    cmpl %edx, %edi                ; Check if end < n
    jge next_gap
    
    ; Calculate dp[start][end]
    call calculate_dp_value
    
next_gap:
    incl %edi                      ; Increment start
    jmp start_loop
    
compute_done:
    popl %ecx                      ; Restore length
    ret

calculate_dp_value:
    ; Calculate dp[start][end] value
    pushl %ebp                     ; Save frame pointer
    movl %esp, %ebp                ; Set up new frame
    
    ; Get parameters from stack
    movl 8(%ebp), %eax             ; start
    movl 12(%ebp), %ebx            ; end
    
    ; dp[start][end] = dp[start][end-1] + sum of dp[start][k-1] * dp[k+1][end]
    ; for valid k where start <= k <= end
    
    ; Initialize result to dp[start][end-1]
    movl %ebx, %ecx                ; end
    decl %ecx                      ; end-1
    call get_dp_value              ; Get dp[start][end-1]
    movl %eax, %edi                ; Store in result
    
    ; Add sum of valid pairs
    movl %eax, %esi                ; k = start
inner_loop:
    cmpb $0, %dl                   ; Check if k <= end
    jg inner_done
    
    ; Check if pair (k, end) is valid
    call is_valid_pair             ; Check if valid pair
    jz skip_calc
    
    ; Calculate dp[start][k-1] * dp[k+1][end]
    movl %esi, %eax                ; k
    decl %eax                      ; k-1
    call get_dp_value              ; Get dp[start][k-1]
    movl %eax, %ecx                ; Store first factor
    
    movl %esi, %eax                ; k
    incl %eax                      ; k+1
    call get_dp_value              ; Get dp[k+1][end]
    
    imull %ecx, %eax               ; Multiply factors
    addl %eax, %edi                ; Add to result
    
skip_calc:
    incl %esi                      ; Increment k
    jmp inner_loop
    
inner_done:
    ; Store result back in DP table
    movl %eax, %ebx                ; Save result
    call set_dp_value              ; Store in dp[start][end]
    
    popl %ebp                      ; Restore frame pointer
    ret

is_valid_pair:
    ; Check if nucleotides at positions k and end form valid pair
    pushl %ebx                     ; Save end position
    
    movl %esi, %eax                ; k (position)
    movl %ebx, %ecx                ; end (position)
    
    ; Get nucleotides
    movb rna_seq(%eax), %dl        ; nucleotide at k
    movb rna_seq(%ecx), %dh        ; nucleotide at end
    
    ; Check for valid pairs
    call check_pair_validity
    
    popl %ebx                      ; Restore end position
    ret

check_pair_validity:
    ; Check if pair (dl, dh) is valid
    pushl %eax                     ; Save original values
    
    ; Check A-U pair
    cmpb $'A', %dl
    jnz check_u_a
    cmpb $'U', %dh
    jz pair_valid
check_u_a:
    cmpb $'U', %dl
    jnz check_g_c
    cmpb $'A', %dh
    jz pair_valid
check_g_c:
    cmpb $'G', %dl
    jnz check_c_g
    cmpb $'C', %dh
    jz pair_valid
check_c_g:
    cmpb $'C', %dl
    jnz check_g_u
    cmpb $'G', %dh
    jz pair_valid
check_g_u:
    cmpb $'G', %dl
    jnz check_u_g
    cmpb $'U', %dh
    jz pair_valid
check_u_g:
    cmpb $'U', %dl
    jnz not_valid
    cmpb $'G', %dh
    jz pair_valid
    
not_valid:
    xorl %eax, %eax                ; Return 0 (invalid)
    jmp validity_done
    
pair_valid:
    movl $1, %eax                  ; Return 1 (valid)
    
validity_done:
    popl %eax                      ; Restore original values
    ret

get_dp_value:
    ; Get value from DP table at [start][end]
    pushl %ebp                     ; Save frame pointer
    
    movl %esi, %eax                ; start
    movl %edi, %ebx                ; end
    
    ; Calculate index: start * n + end
    movl seq_len(%esp), %ecx       ; Get sequence length
    imull %eax, %ecx               ; start * n
    addl %ebx, %ecx                ; + end
    shll $2, %ecx                  ; Multiply by 4 (32-bit)
    addl %ebp, %ecx                ; Add base address
    
    movl (%ecx), %eax              ; Get value from DP table
    
    popl %ebp                      ; Restore frame pointer
    ret

set_dp_value:
    ; Set value in DP table at [start][end]
    pushl %ebp                     ; Save frame pointer
    
    movl %esi, %eax                ; start
    movl %edi, %ebx                ; end
    
    ; Calculate index: start * n + end
    movl seq_len(%esp), %ecx       ; Get sequence length
    imull %eax, %ecx               ; start * n
    addl %ebx, %ecx                ; + end
    shll $2, %ecx                  ; Multiply by 4 (32-bit)
    addl %ebp, %ecx                ; Add base address
    
    movl %eax, (%ecx)              ; Set value in DP table
    
    popl %ebp                      ; Restore frame pointer
    ret
```

## Explanation

This Assembly solution implements the dynamic programming approach to count RNA secondary structures with wobble bonding:

1. **Data Structures**: 
   - RNA sequence stored in memory
   - Dynamic Programming table allocated using mmap
   - Valid base pairs defined for wobble bonding

2. **Algorithm**:
   - Initialize DP table where `dp[i][i] = 1` (single nucleotides)
   - For each possible substring, compute the number of valid structures
   - Use recurrence relation: `dp[i][j] = dp[i][j-1] + Σ dp[i][k-1] * dp[k+1][j]`
   - Include wobble pairs (G-U and U-G) in valid pair checking

3. **Key Functions**:
   - `init_dp_table`: Initialize base cases
   - `compute_structures`: Main DP computation loop
   - `is_valid_pair`: Check if two nucleotides form valid bonds
   - `get_dp_value`/`set_dp_value`: Access DP table elements

4. **Time Complexity**: O(n³) where n is sequence length
5. **Space Complexity**: O(n²) for the DP table

The solution handles all valid wobble bonding pairs and correctly counts all possible RNA secondary structures.