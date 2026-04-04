# Building a Heap in Assembly

I'll solve the Rosalind problem "Building a Heap" which requires converting an array into a max heap using heapify operations.

## Problem Understanding

The task is to:
1. Convert an unsorted array into a max heap
2. Output the indices of swaps performed during heap construction
3. The heap should satisfy the max heap property: parent ≥ children

## Assembly Solution

```assembly
.data
    # Array input (example: 5 1 3 2 4)
    array: .word 5, 1, 3, 2, 4
    array_size: .word 5
    
    # Output buffer
    output_buffer: .space 100
    
    # Format strings
    newline: .ascii "\n"
    space: .ascii " "
    
.text
.globl _start

_start:
    # Load array and size
    la $a0, array
    lw $a1, array_size
    
    # Convert to max heap
    jal build_heap
    
    # Exit program
    li $v0, 10
    syscall

# Function: build_heap
# Parameters: $a0 = array address, $a1 = array size
# Returns: void
build_heap:
    # Calculate last non-leaf node (index = n/2 - 1)
    srl $t0, $a1, 1          # $t0 = n/2
    subi $t0, $t0, 1         # $t0 = n/2 - 1
    
    # Loop from last non-leaf to root
    loop_build_heap:
        beq $t0, $zero, heap_done
        
        # Call heapify on current node
        move $a2, $t0
        jal heapify
        
        # Decrement index
        subi $t0, $t0, 1
        j loop_build_heap
    
    heap_done:
        jr $ra

# Function: heapify
# Parameters: $a0 = array address, $a1 = array size, $a2 = heapify index
# Returns: void
heapify:
    # Save registers
    push $ra
    push $s0
    push $s1
    push $s2
    push $s3
    
    # Initialize variables
    move $s0, $a0            # array address
    move $s1, $a1            # array size
    move $s2, $a2            # current index
    
    # Initialize largest as root
    move $s3, $s2            # largest = current index
    
    # Calculate left child index
    sll $t0, $s2, 1          # $t0 = index * 2
    addi $t0, $t0, 1         # $t0 = 2*index + 1 (left child)
    
    # Check if left child exists and is greater than root
    bge $t0, $s1, right_check
    
    # Load left child value
    sll $t1, $t0, 2          # $t1 = index * 4 (word offset)
    add $t1, $s0, $t1        # $t1 = address of left child
    lw $t2, 0($t1)           # $t2 = left child value
    
    # Load root value
    sll $t3, $s2, 2          # $t3 = index * 4 (word offset)
    add $t3, $s0, $t3        # $t3 = address of root
    lw $t4, 0($t3)           # $t4 = root value
    
    # Compare: if left > root
    bgt $t2, $t4, update_largest
    
    # Check right child
    right_check:
        # Calculate right child index
        addi $t0, $t0, 1     # $t0 = 2*index + 2 (right child)
        
        # Check if right child exists and is greater than current largest
        bge $t0, $s1, heapify_done
        
        # Load right child value
        sll $t1, $t0, 2      # $t1 = index * 4 (word offset)
        add $t1, $s0, $t1    # $t1 = address of right child
        lw $t2, 0($t1)       # $t2 = right child value
        
        # Load largest value
        sll $t3, $s3, 2      # $t3 = index * 4 (word offset)
        add $t3, $s0, $t3    # $t3 = address of largest
        lw $t4, 0($t3)       # $t4 = largest value
        
        # Compare: if right > largest
        bgt $t2, $t4, update_largest
    
    # If largest is not root, swap and continue heapifying
    update_largest:
        # If largest is not current root
        beq $s3, $s2, heapify_done
        
        # Perform swap
        # Load root value
        sll $t0, $s2, 2
        add $t0, $s0, $t0
        lw $t1, 0($t0)
        
        # Load largest value
        sll $t2, $s3, 2
        add $t2, $s0, $t2
        lw $t3, 0($t2)
        
        # Store values back
        sw $t3, 0($t0)   # root = largest
        sw $t1, 0($t2)   # largest = root
        
        # Print swap indices (1-indexed)
        addi $a0, $s2, 1    # root index + 1
        addi $a1, $s3, 1    # largest index + 1
        jal print_swap
        
        # Recursively heapify the affected subtree
        move $a2, $s3
        jal heapify
    
    heapify_done:
        # Restore registers
        pop $s3
        pop $s2
        pop $s1
        pop $s0
        pop $ra
        jr $ra

# Function: print_swap
# Parameters: $a0 = first index, $a1 = second index
# Returns: void
print_swap:
    # Save registers
    push $ra
    push $t0
    push $t1
    
    # Print first index
    li $v0, 1
    move $a0, $a0
    syscall
    
    # Print space
    li $v0, 11
    li $a0, 32          # space character
    syscall
    
    # Print second index
    li $v0, 1
    move $a0, $a1
    syscall
    
    # Print newline
    li $v0, 11
    li $a0, 10          # newline character
    syscall
    
    # Restore registers
    pop $t1
    pop $t0
    pop $ra
    jr $ra

# Stack operations
push:
    addi $sp, $sp, -4
    sw $ra, 0($sp)
    jr $ra

pop:
    lw $ra, 0($sp)
    addi $sp, $sp, 4
    jr $ra
```

## Alternative Simpler Version

```assembly
.data
    # Input array: 5 1 3 2 4
    arr: .word 5, 1, 3, 2, 4
    size: .word 5
    
.text
.globl _start

_start:
    # Initialize
    la $a0, arr
    lw $a1, size
    
    # Build heap from bottom up
    li $t0, 0                # i = 0
    li $t1, 1                # j = 1
    
    # Calculate last parent index
    srl $t2, $a1, 1          # n/2
    subi $t2, $t2, 1         # n/2 - 1
    
    # Main loop
    loop:
        # Check if we've processed all nodes
        bge $t0, $t2, done
        
        # Call heapify for node at index $t0
        move $a2, $t0
        jal heapify
        
        # Increment index
        addi $t0, $t0, 1
        j loop
    
    done:
        # Exit
        li $v0, 10
        syscall

# Heapify function - simplified version
heapify:
    # For demonstration, we'll just print swaps that would occur
    # In a real implementation, this would actually perform the heap operations
    
    # Print indices of swaps (this is where the actual heap operations would be)
    li $v0, 11
    li $a0, 10              # newline
    syscall
    
    jr $ra
```

## Key Concepts

1. **Heap Construction**: Start from the last non-leaf node and heapify all nodes upward
2. **Heapify Operation**: Ensure the max heap property is maintained
3. **Index Calculations**:
   - Parent of node i: (i-1)/2
   - Left child of node i: 2*i + 1
   - Right child of node i: 2*i + 2
4. **Output Format**: Print indices of swapped elements (1-indexed)

## Time Complexity
- Time: O(n log n) for building heap
- Space: O(log n) for recursion stack

This assembly solution demonstrates the heap construction algorithm while working within the constraints of MIPS assembly language syntax and conventions.

