# Rosalind Problem: Linguistic Complexity of a Genome

## Problem Description
The linguistic complexity of a genome is defined as the ratio of the number of distinct substrings to the total number of possible substrings of a given length.

## Solution in Assembly (x86-64)

```assembly
.section .data
    .align 8
    input_string: .ascii "ACGTACGTACGT"
    input_length: .quad 12
    
    # Buffer for substring storage
    substrings_buffer: .space 1000
    
    # Precomputed powers of 4 (for base-4 counting)
    power_table:
        .quad 1      # 4^0 = 1
        .quad 4      # 4^1 = 4
        .quad 16     # 4^2 = 16
        .quad 64     # 4^3 = 64
        .quad 256    # 4^4 = 256
        .quad 1024   # 4^5 = 1024
        .quad 4096   # 4^6 = 4096
        .quad 16384  # 4^7 = 16384
        .quad 65536  # 4^8 = 65536
        .quad 262144 # 4^9 = 262144

.section .text
    .global _start

# Function to calculate linguistic complexity
linguistic_complexity:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = pointer to input string
    # %rsi = length of string
    # %rdx = k (substring length)
    
    # Calculate total possible substrings of length k
    mov %rsi, %rax      # length
    sub %rdx, %rax      # length - k
    inc %rax            # length - k + 1
    
    # Store total possible substrings
    mov %rax, %r8       # total_possible = length - k + 1
    
    # Calculate distinct substrings using hash table approach
    mov $0, %rcx        # substring index counter
    mov $0, %r9         # distinct_count
    
    # Initialize hash table (array of 0s)
    mov $1024, %r10     # hash table size (2^10)
    xor %rax, %rax      # clear rax for zeroing
    
hash_table_init:
    mov %rax, substrings_buffer(%rcx)  # initialize hash table
    inc %rcx
    cmp %r10, %rcx
    jl hash_table_init
    
    # Process each substring of length k
process_substrings:
    # Check if we've processed all substrings
    mov %rsi, %rax
    sub %rdx, %rax
    inc %rax            # total possible substrings
    cmp %rcx, %rax      # compare with counter
    jge process_done
    
    # Extract substring of length k
    mov %rdi, %r11      # base pointer to string
    add %rcx, %r11      # point to start of current substring
    
    # Convert substring to hash value
    call substring_to_hash
    mov %rax, %r12      # hash value
    
    # Hash table lookup and insertion
    mov %r12, %rax
    xor %rdx, %rdx
    mov $1024, %rcx     # hash table size
    xor %r13, %r13      # clear r13 for remainder
    
    # Hash function: hash % table_size
    xor %rdx, %rdx
    mov %r12, %rax
    xor %rax, %rax
    div %rcx            # rax = hash / 1024, rdx = hash % 1024
    
    # Check if position is empty (0) - meaning new substring
    mov substrings_buffer(,%rdx,8), %r14  # load value at hash position
    
    cmp $0, %r14
    je insert_new
    
    # Compare with existing substring
    call compare_substrings
    cmp $0, %rax        # if equal (return 0)
    je next_substring
    
insert_new:
    # Insert new substring
    mov %r12, substrings_buffer(,%rdx,8)  # store hash in table
    inc %r9             # increment distinct count
    
next_substring:
    inc %rcx            # move to next substring
    jmp process_substrings

process_done:
    # Calculate complexity = distinct / total
    mov %r9, %rax       # distinct count
    mov %r8, %rbx       # total possible
    xor %rdx, %rdx      # clear high bits for division
    
    # Floating point division (simplified)
    cvtsi2sd %rax, %xmm0  # convert distinct to double
    cvtsi2sd %rbx, %xmm1  # convert total to double
    divsd %xmm1, %xmm0    # divide
    
    # Store result in memory or return
    movsd %xmm0, %xmm2
    
    pop %rbp
    ret

# Convert substring to hash value
substring_to_hash:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = pointer to substring
    # %rsi = length of substring (k)
    
    xor %rax, %rax      # hash value
    xor %rcx, %rcx      # index counter
    
hash_loop:
    cmp %rsi, %rcx      # compare with k
    jge hash_done
    
    # Get character value (A=0, C=1, G=2, T=3)
    movb (%rdi,%rcx), %dl
    xor %dh, %dh        # clear high byte
    
    cmp $65, %dl        # 'A'
    je hash_A
    cmp $67, %dl        # 'C'  
    je hash_C
    cmp $71, %dl        # 'G'
    je hash_G
    jmp hash_T          # 'T'
    
hash_A: mov $0, %edx; jmp hash_calc
hash_C: mov $1, %edx; jmp hash_calc
hash_G: mov $2, %edx; jmp hash_calc
hash_T: mov $3, %edx; jmp hash_calc
    
hash_calc:
    # hash = hash * 4 + char_value
    mov %rax, %r8
    shl $2, %r8         # multiply by 4 (shift left 2)
    add %rdx, %r8       # add character value
    mov %r8, %rax
    
    inc %rcx            # next character
    jmp hash_loop
    
hash_done:
    pop %rbp
    ret

# Compare two substrings
compare_substrings:
    push %rbp
    mov %rsp, %rbp
    
    # Parameters:
    # %rdi = pointer to first substring
    # %rsi = pointer to second substring  
    # %rdx = length
    
    xor %rax, %rax      # return value (0 = equal)
    xor %rcx, %rcx      # index counter
    
compare_loop:
    cmp %rdx, %rcx      # compare with length
    jge compare_done
    
    movb (%rdi,%rcx), %dl
    movb (%rsi,%rcx), %dh
    
    cmp %dl, %dh
    jne compare_not_equal
    
    inc %rcx            # next character
    jmp compare_loop
    
compare_not_equal:
    mov $1, %rax        # not equal
    
compare_done:
    pop %rbp
    ret

# Main program entry point
_start:
    # Initialize registers
    mov $input_string, %rdi     # input string pointer
    mov $input_length, %rsi     # string length
    mov $3, %rdx                # k = 3
    
    # Call linguistic complexity function
    call linguistic_complexity
    
    # Exit program
    mov $60, %rax               # sys_exit
    mov $0, %rdi                # exit status
    syscall
```

## Explanation

This assembly implementation solves the linguistic complexity problem by:

1. **Input Processing**: Takes a DNA string and substring length k as input
2. **Hash Table Approach**: Uses a hash table to efficiently track distinct substrings
3. **Substring Hashing**: Converts each substring to a base-4 numeric value using:
   - A → 0, C → 1, G → 2, T → 3
   - Combines character values using base-4 arithmetic
4. **Distinct Counting**: Uses hash table collisions to count unique substrings
5. **Complexity Calculation**: Computes ratio of distinct substrings to total possible substrings

## Key Features

- **Efficient Memory Usage**: Uses hash table for O(1) lookup/insertion
- **Base-4 Arithmetic**: Properly handles DNA alphabet conversion
- **Assembly Optimizations**: Minimal overhead, direct memory operations
- **Proper Exit Handling**: Follows Linux system call conventions

## Time Complexity
O(n×k) where n is string length and k is substring length

## Space Complexity  
O(1) for hash table (fixed size) + O(n×k) for storage

The program calculates the linguistic complexity as the ratio of distinct substrings to total possible substrings of given length k.