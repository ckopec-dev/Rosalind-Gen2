# Rosalind Problem: Longest Increasing Subsequence (LIS) in Assembly

I'll solve this problem using x86-64 assembly with Linux system calls. The goal is to find the length of the longest increasing subsequence.

## Problem Analysis

The longest increasing subsequence problem can be solved efficiently using dynamic programming with binary search, but for simplicity in assembly, I'll implement a O(n²) solution that's easier to translate to assembly.

## Assembly Solution

```assembly
.section .data
    # Input array (example: 5 2 8 6 3 6 9 7)
    input_array: .long 5, 2, 8, 6, 3, 6, 9, 7
    array_size: .long 8
    
    # Format strings for output
    newline: .ascii "\n"
    result_format: .ascii "Length of LIS: %d\n"

.section .text
    .global _start

_start:
    # Load array and size
    movl array_size(%rip), %ecx        # ecx = array size
    movl $0, %edi                      # edi = i (outer loop counter)
    
    # Allocate memory for dp array (size = array_size)
    # We'll use stack space for simplicity
    movl %ecx, %eax                    # eax = array size
    shll $2, %eax                      # eax = array size * 4 (bytes)
    subl %eax, %esp                    # Allocate stack space
    
    # Initialize dp array with 1s
init_dp:
    cmpl %ecx, %edi                    # compare i with array_size
    jge init_done                      # if i >= size, done
    
    movl $1, (%esp,%edi,4)             # dp[i] = 1
    incl %edi                          # i++
    jmp init_dp
    
init_done:
    # Reset i for outer loop
    movl $0, %edi                      # i = 0
    
outer_loop:
    cmpl %ecx, %edi                    # compare i with array_size
    jge outer_done                     # if i >= size, done
    
    # Initialize j = i + 1
    movl %edi, %esi                    # esi = i
    incl %esi                          # esi = i + 1
    
inner_loop:
    cmpl %ecx, %esi                    # compare j with array_size
    jge inner_done                     # if j >= size, done
    
    # Load arr[i] and arr[j]
    movl input_array(,%edi,4), %eax    # eax = arr[i]
    movl input_array(,%esi,4), %ebx    # ebx = arr[j]
    
    # If arr[i] < arr[j], update dp[j]
    cmpl %ebx, %eax                    # compare arr[i] and arr[j]
    jge skip_update                    # if arr[i] >= arr[j], skip
    
    # dp[j] = max(dp[j], dp[i] + 1)
    movl (%esp,%esi,4), %edx           # edx = dp[j]
    movl (%esp,%edi,4), %eax           # eax = dp[i]
    incl %eax                          # eax = dp[i] + 1
    cmpl %edx, %eax                    # compare dp[i]+1 and dp[j]
    jle skip_update                    # if dp[i]+1 <= dp[j], skip
    
    movl %eax, (%esp,%esi,4)           # dp[j] = dp[i] + 1
    
skip_update:
    incl %esi                          # j++
    jmp inner_loop
    
inner_done:
    incl %edi                          # i++
    jmp outer_loop
    
outer_done:
    # Find maximum value in dp array
    movl $0, %edi                      # i = 0
    movl $1, %edx                      # max = 1 (minimum possible)
    
find_max:
    cmpl %ecx, %edi                    # compare i with array_size
    jge done                           # if i >= size, done
    
    movl (%esp,%edi,4), %eax           # eax = dp[i]
    cmpl %edx, %eax                    # compare dp[i] and max
    jle skip_max_update                # if dp[i] <= max, skip
    
    movl %eax, %edx                    # max = dp[i]
    
skip_max_update:
    incl %edi                          # i++
    jmp find_max
    
done:
    # Print result
    pushl %edx                         # push max value (result)
    pushl $result_format               # push format string
    call printf
    addl $8, %esp                      # clean up stack
    
    # Exit program
    movl $1, %eax                      # sys_exit
    movl $0, %ebx                      # exit status
    int $0x80

# External function for printf (this would normally be linked)
.section .bss
    .comm buffer, 1024
```

## Key Implementation Details

### Algorithm Approach:
1. **Dynamic Programming**: Use `dp[i]` to represent the length of the longest increasing subsequence ending at index `i`
2. **Initialization**: Set all `dp[i] = 1` (each element forms a subsequence of length 1)
3. **Recurrence Relation**: For each pair `(i,j)` where `i < j`:
   - If `arr[i] < arr[j]`, then `dp[j] = max(dp[j], dp[i] + 1)`

### Assembly Features Used:
- **Memory Management**: Stack allocation for the DP array
- **Loops**: Nested loops using conditional jumps
- **Array Access**: Using base + index addressing mode
- **Function Calls**: printf for output (simplified version)

### Time Complexity:
- **Time**: O(n²) - nested loops
- **Space**: O(n) - DP array storage

## Expected Output
For the input array [5, 2, 8, 6, 3, 6, 9, 7], the longest increasing subsequence is [2, 3, 6, 7] or [2, 3, 6, 9], so the output should be:
```
Length of LIS: 4
```

## Compilation and Execution

```bash
# Assemble and link
as -64 lis.s -o lis.o
ld lis.o -o lis

# Run
./lis
```

This assembly implementation demonstrates the core algorithmic approach to solving the longest increasing subsequence problem in a low-level language environment.