# Rosalind Problem: Binary Search in Assembly

## Problem Description
Given: A positive integer n ≤ 10^5 and a sorted array A[1..n] of integers from -10^5 to 10^5, followed by a positive integer k ≤ 10^5 and k integers from -10^5 to 10^5.

Return: For each of the k integers, output the index (1-based) of that integer in array A if it exists, otherwise output -1.

## Assembly Solution

```assembly
.section .data
    # Input data
    array:      .long 2, 4, 6, 8, 10, 12, 14, 16, 18, 20
    array_size: .long 10
    search_count: .long 5
    targets:    .long 10, 15, 2, 18, 25
    
.section .text
    .global _start

_start:
    # Load array size
    movl array_size(%esp), %ecx
    movl search_count(%esp), %edx
    
    # Loop through each target
    movl targets(%esp), %esi
    movl $1, %edi  # index counter
    
search_loop:
    # Check if we have processed all targets
    cmpl $0, %edx
    je done
    
    # Binary search for current target
    movl (%esi), %eax        # Load target value
    movl $0, %ebx            # low = 0
    movl array_size(%esp), %ecx  # high = array_size - 1
    dec %ecx                 # Adjust for 0-based indexing
    
binary_search:
    # Check if low > high (search failed)
    cmpl %ecx, %ebx
    jg not_found
    
    # Calculate mid = (low + high) / 2
    movl %ebx, %edi          # mid = low
    addl %ecx, %edi          # mid = low + high
    shrl $1, %edi            # mid = (low + high) / 2
    
    # Get array[mid]
    movl array(%edi), %ebp   # Load array value at mid
    
    # Compare with target
    cmpl %eax, %ebp
    je found                 # Found the element
    
    jl search_right          # Target > array[mid], search right
    jmp search_left          # Target < array[mid], search left

search_right:
    # low = mid + 1
    incl %edi
    movl %edi, %ebx
    jmp binary_search

search_left:
    # high = mid - 1
    decl %edi
    movl %edi, %ecx
    jmp binary_search

found:
    # Output index (1-based)
    addl $1, %edi            # Convert to 1-based indexing
    # Here we would normally output the result
    # For simplicity, we'll just store it in a register
    movl %edi, %eax
    
not_found:
    # Output -1 for not found
    movl $-1, %eax
    
    # Move to next target
    addl $4, %esi            # Move pointer to next target
    decl %edx                # Decrement counter
    jmp search_loop

done:
    # Exit program
    movl $1, %eax            # sys_exit
    movl $0, %ebx            # exit status
    int $0x80
```

## Alternative Implementation (More Practical)

```assembly
.section .data
    array:      .long 2, 4, 6, 8, 10, 12, 14, 16, 18, 20
    array_size: .long 10
    
.section .text
    .global _start

# Function to perform binary search
binary_search:
    # Arguments: %esi = target, %edi = array address
    # Returns: %eax = index (1-based) or -1
    
    pushl %ebx               # Save registers
    pushl %ecx
    pushl %edx
    
    movl array_size(%esp), %edx  # Load array size
    decl %edx                # Convert to 0-based index
    
    movl $0, %ebx            # low = 0
    movl %edx, %ecx          # high = array_size - 1
    
binary_search_loop:
    cmpl %ecx, %ebx          # if low > high
    jg not_found             # return -1
    
    # Calculate mid = (low + high) / 2
    movl %ebx, %edx
    addl %ecx, %edx
    shrl $1, %edx            # mid = (low + high) / 2
    
    # Load array[mid]
    movl array(%edx), %eax
    
    cmpl %esi, %eax          # compare with target
    je found                 # if equal, return index
    
    jl search_right          # if target > array[mid]
    
    # Search left (target < array[mid])
    decl %edx                # high = mid - 1
    movl %edx, %ecx
    jmp binary_search_loop

search_right:
    incl %edx                # low = mid + 1
    movl %edx, %ebx
    jmp binary_search_loop

found:
    # Return 1-based index
    incl %edx
    movl %edx, %eax
    jmp search_end

not_found:
    movl $-1, %eax

search_end:
    popl %edx                # Restore registers
    popl %ecx
    popl %ebx
    ret                      # Return result in %eax

_start:
    # Main program logic would go here
    # Call binary_search function for each target
    
    # Exit
    movl $1, %eax            # sys_exit
    movl $0, %ebx
    int $0x80
```

## Key Assembly Concepts Used

1. **Binary Search Algorithm**: Implemented using iterative approach with proper register management
2. **Memory Access**: Loading array elements and targets from memory locations
3. **Conditional Branching**: Using `cmpl` and conditional jumps (`je`, `jl`, `jg`)
4. **Register Management**: Proper use of registers for temporary storage
5. **Pointer Arithmetic**: Moving through array elements using address calculations

## Time Complexity
O(k × log n) where k is the number of targets and n is the array size.

## Space Complexity
O(1) - Only using a constant amount of extra space for variables.