# Rosalind Problem: Counting Inversions - Assembly Solution

I'll solve this step by step using assembly language, assuming we're working with a simple x86-64 assembly approach.

## Problem Understanding

Counting inversions means counting pairs (i,j) where i < j but arr[i] > arr[j]. This is a classic algorithmic problem that can be solved efficiently using merge sort.

## Assembly Solution

```assembly
.section .data
    # Input array (example)
    array: .long 1, 3, 5, 2, 4, 6
    array_size: .long 6
    
    # Temporary array for merge sort
    temp_array: .space 24  # 6 * 4 bytes

.section .text
    .global _start

count_inversions:
    # Function to count inversions using merge sort approach
    # Arguments: rdi = array pointer, rsi = left index, rdx = right index
    # Returns: inversion count in rax
    
    push rbp
    mov rbp, rsp
    
    # Base case: if left >= right, no inversions
    cmp rsi, rdx
    jge end_count
    
    # Calculate mid point
    mov r8, rsi
    add r8, rdx
    shr r8, 1           # r8 = (left + right) / 2
    
    # Recursive calls for left and right halves
    mov rdi, array      # array pointer
    mov rax, rsi        # left index
    mov rbx, r8         # mid index
    call count_inversions
    
    mov r9, rax         # save left inversions
    
    mov rdi, array      # array pointer  
    mov rax, r8         # mid index + 1
    add rax, 1
    mov rbx, rdx        # right index
    call count_inversions
    
    add r9, rax         # add right inversions
    
    # Count split inversions
    mov rdi, array
    mov rsi, rax        # left index  
    mov rdx, r8         # mid index
    mov rcx, rdx        # mid index
    add rcx, 1          # left side of right half
    mov r8, rdx         # save mid for merge
    call merge_and_count
    
    add r9, rax         # add split inversions
    
end_count:
    mov rax, r9         # return total inversions
    pop rbp
    ret

merge_and_count:
    # Merge two sorted arrays and count inversions
    # rdi = array pointer, rsi = left, rdx = mid, rcx = right
    
    push rbp
    mov rbp, rsp
    
    # Copy to temporary array
    mov r8, rsi         # left index
    mov r9, 0           # temp index
    
    # Copy left half
    mov r10, rdx
    sub r10, rsi        # length of left half
    add r10, 1          # +1 for inclusive
    
copy_left:
    cmp r9, r10
    jge copy_right
    
    mov eax, [rdi + r8 * 4]
    mov [temp_array + r9 * 4], eax
    inc r8
    inc r9
    jmp copy_left

copy_right:
    # Copy right half to temp array
    mov r8, rcx         # right index
    mov r10, rdx        # mid index
    
    # Calculate length of right half
    sub r8, r10
    add r8, 1           # +1 for inclusive
    dec r8              # adjust for 0-based indexing
    
copy_right_loop:
    cmp r9, r8
    jge merge_loop
    
    mov eax, [rdi + rcx * 4]
    mov [temp_array + r9 * 4], eax
    inc rcx
    inc r9
    jmp copy_right_loop

merge_loop:
    # Merge and count inversions
    mov r8, rsi         # left index in temp
    mov r9, rdx         # mid index in temp  
    add r9, 1           # start of right half
    mov r10, rcx        # right end index
    
    # Initialize counters
    mov rax, 0          # inversion count
    mov r11, rsi        # i (left pointer)
    mov r12, r9         # j (right pointer)
    
merge_step:
    # Check if we've processed all elements
    cmp r11, rdx        # left index < mid?
    jg merge_right_only
    
    cmp r12, r10        # right index <= right end?
    jg merge_left_only
    
    # Compare elements from both arrays
    mov eax, [temp_array + r11 * 4]  # temp[i]
    mov ebx, [temp_array + r12 * 4]  # temp[j]
    
    cmp eax, ebx        # if left <= right
    jle merge_left_only
    
    # Inversion found: left > right
    # Count inversions: (mid - i + 1) inversions
    mov r13, rdx
    sub r13, r11
    add r13, 1          # number of elements from i to mid
    add rax, r13
    
    # Move right element to result
    mov eax, [temp_array + r12 * 4]
    mov [rdi + r11 * 4], eax
    inc r12
    jmp merge_step

merge_left_only:
    # Copy remaining left elements
    cmp r11, rdx
    jg merge_right_only
    
    mov eax, [temp_array + r11 * 4]
    mov [rdi + r11 * 4], eax
    inc r11
    jmp merge_step

merge_right_only:
    # Copy remaining right elements
    cmp r12, r10
    jg copy_back
    
    mov eax, [temp_array + r12 * 4]
    mov [rdi + r12 * 4], eax
    inc r12
    jmp merge_step

copy_back:
    # Copy back to original array (if needed)
    mov r8, rsi         # left index
    mov r9, 0           # temp index
    
    copy_back_loop:
        cmp r9, rcx     # temp_index < right?
        jg end_merge
        
        mov eax, [temp_array + r9 * 4]
        mov [rdi + r8 * 4], eax
        inc r8
        inc r9
        jmp copy_back_loop

end_merge:
    pop rbp
    ret

# Main function to solve the problem
solve_inversion_problem:
    push rbp
    mov rbp, rsp
    
    # Initialize parameters
    mov rdi, array      # array pointer
    mov rsi, 0          # left = 0
    mov rdx, array_size # right = size - 1
    
    # Call count_inversions function
    call count_inversions
    
    # Result is in rax
    
    pop rbp
    ret

# Entry point
_start:
    # Initialize stack and call solver
    call solve_inversion_problem
    
    # Exit program
    mov rax, 60         # sys_exit
    mov rdi, 0          # exit status
    syscall
```

## Algorithm Explanation

This assembly implementation uses the **divide-and-conquer approach** with merge sort:

1. **Divide**: Split array into two halves
2. **Conquer**: Recursively count inversions in each half
3. **Combine**: Count split inversions (where one element is in left half, other in right)

## Key Concepts

- **Inversion**: Pair (i,j) where i < j but arr[i] > arr[j]
- **Time Complexity**: O(n log n) - same as merge sort
- **Space Complexity**: O(n) for temporary array

## Usage Notes

This is a conceptual implementation. Actual usage would require:
- Proper linking with system calls
- Input handling from stdin or file
- Error checking and validation
- Optimized memory management

The solution handles the core algorithm efficiently using assembly's low-level control over memory and computation, making it suitable for performance-critical applications where Rosalind's constraints might apply.