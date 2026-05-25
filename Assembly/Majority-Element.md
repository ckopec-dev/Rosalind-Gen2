# Rosalind Problem: Majority Element in Assembly

## Problem Understanding

The Majority Element problem asks us to find an element that appears more than n/2 times in an array of n elements. In Assembly language, we need to implement this using low-level operations.

## Approach

We'll use the **Boyer-Moore Majority Vote Algorithm** which works in O(n) time and O(1) space:
1. Find a candidate element (majority candidate)
2. Verify if it's actually the majority element

## Assembly Implementation (x86-64)

```assembly
.section .data
    # Input array (example)
    array: .long 3, 3, 4, 2, 4, 4, 2, 4, 4
    array_size: .long 9

.section .text
    .global _start

majority_element:
    # Function to find majority element
    # Input: array pointer in RDI, size in RSI
    # Output: majority element in RAX
    
    # Initialize candidate and count
    mov rax, [rdi]          # Load first element as candidate
    mov rcx, 1              # count = 1
    
    # Iterate through remaining elements
    mov r8, 1               # i = 1
    mov r9, [rsi]           # n = array_size
    
    # Loop to find candidate
find_candidate:
    cmp r8, r9              # if i >= n
    jge verify_candidate
    
    mov r10, [rdi + r8*4]  # array[i]
    
    cmp r10, rax            # if array[i] == candidate
    je increment_count
    
    # Decrement count
    dec rcx
    cmp rcx, 0
    jg check_next
    
    # Reset candidate
    mov rax, r10
    mov rcx, 1
    
check_next:
    inc r8
    jmp find_candidate

increment_count:
    inc rcx
    inc r8
    jmp find_candidate

verify_candidate:
    # Verify the candidate is actually majority
    mov r8, 0
    mov rcx, 0              # count = 0
    
verify_loop:
    cmp r8, r9
    jge done
    
    mov r10, [rdi + r8*4]
    cmp r10, rax
    jne skip_increment
    
    inc rcx
    
skip_increment:
    inc r8
    jmp verify_loop

done:
    # Check if candidate appears more than n/2 times
    mov r10, r9
    shr r10, 1              # n/2
    cmp rcx, r10
    jg return_candidate
    mov rax, -1             # No majority element
    
return_candidate:
    ret

# Main function to test
_start:
    # Call majority_element function
    lea rdi, array          # Load array address
    mov rsi, array_size     # Load array size
    call majority_element
    
    # Exit program
    mov rax, 60             # sys_exit
    mov rdi, 0              # exit status
    syscall
```

## Alternative Implementation (Simplified)

```assembly
.section .data
    # Simple test array
    test_array: .long 3, 3, 4, 2, 4, 4, 2, 4, 4
    array_length: .long 9

.section .text
    .global _start

# Simplified Boyer-Moore implementation
majority_element_simple:
    # Input: array in RDI, length in RSI
    # Output: majority element in RAX
    
    # Initialize
    mov rax, [rdi]          # First element as candidate
    mov rcx, 1              # Count = 1
    
    # Loop through array
    mov r8, 1               # i = 1
    mov r9, [rsi]           # n = length
    
loop_start:
    cmp r8, r9
    jge verify
    
    # Load current element
    mov r10, [rdi + r8*4]
    
    # Compare with candidate
    cmp r10, rax
    je same_element
    
    # Different element - decrement count
    dec rcx
    cmp rcx, 0
    jg continue_loop
    
    # Reset candidate
    mov rax, r10
    mov rcx, 1
    
continue_loop:
    inc r8
    jmp loop_start

same_element:
    inc rcx
    inc r8
    jmp loop_start

verify:
    # Verify actual majority
    mov r8, 0
    mov rcx, 0              # Reset count
    
verify_loop:
    cmp r8, [rsi]
    jge not_found
    
    mov r10, [rdi + r8*4]
    cmp r10, rax
    jne skip
    
    inc rcx
    
skip:
    inc r8
    jmp verify_loop

not_found:
    mov rax, -1             # No majority found
    ret

_start:
    lea rdi, test_array
    mov rsi, array_length
    call majority_element_simple
    
    # Exit
    mov rax, 60
    mov rdi, 0
    syscall
```

## Key Assembly Concepts Used

1. **Memory Access**: Using `mov` instructions to load/store array elements
2. **Pointer Arithmetic**: `rdi + r8*4` for array indexing (4 bytes per integer)
3. **Conditional Branching**: `cmp`, `je`, `jg`, `jge` for control flow
4. **Register Usage**: 
   - RDI: array pointer
   - RSI: array size
   - RAX: candidate element
   - RCX: counter
   - R8, R9, R10: loop counters and temporary values

## Time and Space Complexity

- **Time Complexity**: O(n) - two passes through the array
- **Space Complexity**: O(1) - only using a constant amount of extra memory

## Expected Output

For the test array `[3, 3, 4, 2, 4, 4, 2, 4, 4]`, the majority element is `4` since it appears 5 times out of 9 elements (5 > 9/2 = 4.5).

