# Rosalind Problem: Heap Sort in Assembly

I'll solve the Heap Sort problem in Assembly language, implementing the algorithm to sort an array using a max heap.

## Problem Understanding

The task is to implement heap sort on an array of integers. Heap sort works by:
1. Building a max heap from the input array
2. Repeatedly extracting the maximum element and rebuilding the heap

## Assembly Implementation (x86-64)

```assembly
.section .data
    # Array to be sorted (example)
    array: .long 4, 10, 3, 5, 1
    array_size: .long 5

.section .text
    .global _start

# Function to heapify a subtree rooted at index i
# Parameters: rdi = array pointer, rsi = heap size, rdx = root index
heapify:
    push rbp
    mov rbp, rsp
    
    mov r8, rdx           # root = i
    mov r9, rdx           # largest = i
    mov r10, rdx          # left = 2*i + 1
    inc r10
    shl r10, 1            # left = 2*(i+1) - 1
    
    # Check if left child exists and is greater than root
    mov r11, rsi
    dec r11               # heap_size - 1
    cmp r10, r11
    jg heapify_right      # if left >= heap_size, skip
    
    mov rax, [rdi + r10*4]  # arr[left]
    mov rbx, [rdi + r9*4]   # arr[largest]
    cmp rax, rbx
    jle heapify_right
    
    mov r9, r10           # largest = left
    
heapify_right:
    mov r10, rdx          # right = 2*i + 2
    inc r10
    inc r10               # right = 2*(i+1)
    
    # Check if right child exists and is greater than root
    cmp r10, rsi
    jge heapify_swap      # if right >= heap_size, skip
    
    mov rax, [rdi + r10*4]  # arr[right]
    mov rbx, [rdi + r9*4]   # arr[largest]
    cmp rax, rbx
    jle heapify_swap
    
    mov r9, r10           # largest = right
    
heapify_swap:
    # If largest is not root
    cmp r8, r9
    je heapify_end
    
    # Swap arr[i] and arr[largest]
    mov rax, [rdi + r8*4]
    mov rbx, [rdi + r9*4]
    
    mov [rdi + r8*4], rbx
    mov [rdi + r9*4], rax
    
    # Recursively heapify the affected sub-tree
    mov rdx, r9           # i = largest
    call heapify
    
heapify_end:
    pop rbp
    ret

# Function to build max heap
# Parameters: rdi = array pointer, rsi = heap size
build_heap:
    push rbp
    mov rbp, rsp
    
    # Start from last non-leaf node and heapify each node
    mov rax, rsi
    dec rax               # heap_size - 1
    shr rax, 1            # (heap_size - 1) / 2
    mov rcx, rax          # i = (heap_size - 1) / 2
    
build_loop:
    cmp rcx, -1
    je build_end
    
    # Call heapify for node i
    push rcx
    push rsi
    push rdi
    call heapify
    pop rdi
    pop rsi
    pop rcx
    
    dec rcx
    jmp build_loop
    
build_end:
    pop rbp
    ret

# Main heap sort function
heap_sort:
    push rbp
    mov rbp, rsp
    
    # Build max heap
    call build_heap
    
    # One by one extract elements from heap
    mov rax, rsi          # i = heap_size - 1
    dec rax
    mov rcx, rax          # i = heap_size - 1
    
sort_loop:
    cmp rcx, 0
    jle sort_end
    
    # Move current root to end
    mov rax, [rdi]        # arr[0]
    mov rbx, [rdi + rcx*4] # arr[i]
    
    mov [rdi], rbx        # arr[0] = arr[i]
    mov [rdi + rcx*4], rax # arr[i] = arr[0]
    
    # Call heapify on the reduced heap
    dec rsi               # heap_size = heap_size - 1
    push rcx
    push rsi
    push rdi
    call heapify
    pop rdi
    pop rsi
    pop rcx
    
    dec rcx
    jmp sort_loop
    
sort_end:
    pop rbp
    ret

# Main function
_start:
    # Initialize parameters for heap_sort
    mov rdi, array        # array pointer
    mov rsi, array_size   # heap size
    
    # Call heap sort
    push rsi
    push rdi
    call heap_sort
    pop rdi
    pop rsi
    
    # Exit program
    mov rax, 60           # sys_exit
    mov rdi, 0            # exit status
    syscall
```

## Algorithm Explanation

### Heap Sort Steps:

1. **Build Max Heap**: Convert input array into a max heap where parent nodes are greater than their children
2. **Extract Elements**: Repeatedly remove the maximum element (root) and place it at the end
3. **Heapify**: After each extraction, restore the heap property

### Key Functions:

- **heapify()**: Maintains the heap property by comparing a node with its children
- **build_heap()**: Converts array into max heap structure
- **heap_sort()**: Implements complete heap sort algorithm

### Time Complexity:
- Building heap: O(n)
- Sorting: O(n log n)
- Overall: O(n log n)

### Space Complexity:
- O(1) - sorts in-place

## Example Usage

For input array `[4, 10, 3, 5, 1]`:
1. Build max heap: `[10, 5, 3, 4, 1]`
2. Extract elements: `[10, 5, 4, 3, 1]`
3. Final sorted array: `[1, 3, 4, 5, 10]`

This implementation uses x86-64 assembly syntax and follows the standard heap sort algorithm principles.