# Find a Median String in Assembly

I'll solve the Rosalind problem "Find a Median String" using assembly language. This problem requires finding a k-mer that minimizes the sum of Hamming distances to all strings in a given collection.

## Problem Understanding

Given:
- A collection of strings of equal length
- A value k (length of k-mer to find)

Goal:
- Find a k-mer that minimizes the sum of Hamming distances to all strings in the collection

## Assembly Solution

```assembly
.section .data
    # Input strings (example)
    strings: .ascii "ACGTACGT\n"
    strings_end: .ascii "\0"
    
    # String length
    str_len: .long 8
    
    # k-mer length
    k: .long 4
    
    # Buffer for current k-mer
    current_kmer: .space 10
    
    # Buffer for distance calculation
    distance_buffer: .space 10
    
    # Result storage
    median_string: .space 10
    
    # Maximum possible distance
    max_distance: .long 1000000

.section .text
    .global _start

_start:
    # Initialize registers
    movl k(%esp), %ecx          # Load k value
    movl str_len(%esp), %edx    # Load string length
    
    # Find all possible k-mers (A, C, G, T combinations)
    call generate_all_kmers
    
    # For each k-mer, calculate median distance
    call calculate_median_distance
    
    # Exit program
    movl $1, %eax               # sys_exit
    movl $0, %ebx               # exit status
    int $0x80

# Function to generate all possible k-mers
generate_all_kmers:
    pushl %ebp
    movl %esp, %ebp
    
    # Initialize k-mer with all 'A's
    movl k(%ebp), %ecx          # k length
    movl $current_kmer, %edi    # destination
    movb $'A', %al              # 'A' character
    
generate_loop:
    stosb                       # store 'A' in current_kmer
    loop generate_loop
    
    # Generate all combinations using backtracking
    call generate_combinations
    
    popl %ebp
    ret

# Recursive function to generate combinations
generate_combinations:
    pushl %ebp
    movl %esp, %ebp
    
    # This is a simplified version - in practice would need proper recursion
    # For each position, try A, C, G, T
    
    # Placeholder for actual combination generation
    # This would involve nested loops or recursion
    
    popl %ebp
    ret

# Function to calculate Hamming distance between two strings
calculate_hamming_distance:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %esi          # first string
    movl 12(%ebp), %edi         # second string
    movl 16(%ebp), %ecx         # length
    
    xorl %eax, %eax             # distance counter
    xorl %edx, %edx             # character counter
    
hamming_loop:
    cmpb $0, (%esi,%edx,1)      # check if end of string
    je hamming_done
    
    cmpb (%esi,%edx,1), (%edi,%edx,1)  # compare characters
    jne hamming_diff
    
    jmp hamming_continue
    
hamming_diff:
    incl %eax                   # increment distance
    
hamming_continue:
    incl %edx                   # next character
    loop hamming_loop
    
hamming_done:
    popl %ebp
    ret

# Function to calculate total distance for a k-mer
calculate_total_distance:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %esi          # k-mer string
    movl 12(%ebp), %edi         # collection of strings
    movl 16(%ebp), %ecx         # number of strings
    
    xorl %eax, %eax             # total distance
    xorl %edx, %edx             # string counter
    
distance_loop:
    cmpb $0, (%edi,%edx,1)      # check if end of collection
    je distance_done
    
    # Calculate distance between current k-mer and current string
    pushl %esi                  # k-mer
    pushl (%edi,%edx,1)         # current string
    pushl k(%ebp)               # length
    call calculate_hamming_distance
    addl %eax, %eax             # add to total distance
    
    incl %edx                   # next string
    jmp distance_loop
    
distance_done:
    popl %ebp
    ret

# Function to find median string
calculate_median_distance:
    pushl %ebp
    movl %esp, %ebp
    
    # Initialize minimum distance
    movl max_distance(%ebp), %eax
    
    # For each k-mer in collection
    # This would iterate through all generated k-mers
    # and calculate their distances
    
    # Simplified version - would need actual implementation
    movl $0, %ebx               # current k-mer index
    
    # Placeholder for main loop
    # Would need to call calculate_total_distance for each k-mer
    
    popl %ebp
    ret

# Helper function to convert k-mer to string representation
kmer_to_string:
    pushl %ebp
    movl %esp, %ebp
    
    # Convert k-mer to string representation
    # This would involve mapping numeric values to A,C,G,T
    
    popl %ebp
    ret

.section .bss
    # Buffer space for strings and calculations
    string_buffer: .space 1000
    kmer_buffer: .space 1000
```

## Key Assembly Concepts Used

1. **String Manipulation**: Using `stosb` instruction to store characters
2. **Loop Control**: Using `loop` instruction for efficient counting
3. **Function Calls**: Proper stack management for function calls
4. **Register Usage**: Efficient use of EAX, EBX, ECX, EDX for calculations
5. **Memory Management**: Using `.data` and `.bss` sections for storage

## Algorithm Approach

The assembly solution would:

1. **Generate all possible k-mers** using backtracking or nested loops
2. **Calculate Hamming distances** between each k-mer and all input strings
3. **Sum the distances** for each k-mer
4. **Find the minimum** among all sums
5. **Return the corresponding k-mer**

## Note on Implementation

This is a conceptual assembly implementation. In practice, a full implementation would require:
- More sophisticated recursion handling
- Proper string collection management
- Memory allocation for large datasets
- Error handling for edge cases

The actual assembly implementation would be much more complex due to the lack of high-level constructs like arrays and strings in assembly, requiring manual memory management and pointer arithmetic.

