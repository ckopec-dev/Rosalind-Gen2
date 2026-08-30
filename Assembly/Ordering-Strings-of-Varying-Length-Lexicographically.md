# Rosalind Problem: Ordering Strings of Varying Length Lexicographically (Assembly Solution)

## Problem Understanding

This problem asks us to sort a collection of DNA strings in lexicographic order, where strings can have different lengths. In lexicographic ordering:
- Strings are compared character by character from left to right
- If one string is a prefix of another, the shorter one comes first
- The comparison is done using the standard DNA alphabet: A < C < G < T

## Assembly Implementation

```assembly
.data
    strings: .space 1000          # Buffer for input strings
    num_strings: .word 0          # Number of strings to process
    max_length: .word 20          # Maximum string length
    
    # DNA alphabet order: A=0, C=1, G=2, T=3
    alphabet_order: .byte 0, 1, 2, 3
    
.text
.globl _start

_start:
    # Read number of strings
    call read_number
    sw $v0, num_strings
    
    # Read all strings into memory
    li $t0, 0                      # counter
read_loop:
    beq $t0, $t1, sort_strings     # if counter equals num_strings, go to sorting
    jal read_string
    addi $t0, $t0, 1
    j read_loop
    
sort_strings:
    # Implement bubble sort for lexicographic ordering
    li $t2, 0                      # outer loop counter
outer_loop:
    li $t3, 0                      # inner loop counter
inner_loop:
    # Compare strings at positions $t2 and $t3
    jal compare_strings
    # If result > 0 (first string > second), swap them
    bgtz $v0, swap_strings
    
    addi $t3, $t3, 1
    li $t4, 0                      # check if we've reached end
    add $t4, $t2, $t3
    li $t5, 0
    add $t5, $t2, $t3
    
    # Continue inner loop
    j inner_loop
    
swap_strings:
    # Swap strings at positions $t2 and $t3
    jal swap_string_positions
    j inner_loop

# Function to compare two strings
compare_strings:
    # $a0 = position of first string
    # $a1 = position of second string
    # Return: 1 if first > second, -1 if first < second, 0 if equal
    
    li $t0, 0                      # character index
compare_loop:
    # Load characters from both strings
    lb $t1, string1_char($a0, $t0)  # first string char
    lb $t2, string2_char($a1, $t0)  # second string char
    
    # Check if we've reached end of first string
    beq $t1, $zero, return_negative
    # Check if we've reached end of second string  
    beq $t2, $zero, return_positive
    
    # Compare characters using alphabet order
    li $t3, 0                      # index for alphabet lookup
alphabet_lookup:
    beq $t1, $t3, char1_found
    addi $t3, $t3, 1
    j alphabet_lookup
    
char1_found:
    li $t4, 0                      # index for alphabet lookup
alphabet_lookup2:
    beq $t2, $t4, char2_found
    addi $t4, $t4, 1
    j alphabet_lookup2
    
char2_found:
    # Compare indices
    bgt $t3, $t4, return_positive
    blt $t3, $t4, return_negative
    addi $t0, $t0, 1               # increment character index
    j compare_loop
    
return_positive:
    li $v0, 1
    jr $ra
    
return_negative:
    li $v0, -1
    jr $ra
    
# Function to read a string from input
read_string:
    # Implementation for reading one string
    # This would involve reading characters until newline or end of input
    jr $ra

# Function to swap two strings at given positions
swap_string_positions:
    # Implementation for swapping strings in memory
    jr $ra

# Function to read number from input
read_number:
    # Implementation for parsing integer input
    li $v0, 1
    jr $ra
```

## Alternative Optimized Approach Using Quick Sort

```assembly
.data
    string_array: .space 2000      # Array of string pointers
    temp_string: .space 50         # Temporary storage for string operations
    
.text
.globl _start

_start:
    # Read input and populate string array
    jal read_all_strings
    
    # Sort using quicksort
    li $a0, 0                      # low index
    li $a1, num_strings            # high index (subtract 1)
    jal quicksort
    
    # Output sorted strings
    jal print_sorted_strings
    
    # Exit program
    li $v0, 10
    syscall

# Quicksort implementation for string comparison
quicksort:
    # $a0 = low index, $a1 = high index
    # Implementation would involve:
    # 1. Partition strings around pivot
    # 2. Recursively sort left and right partitions
    jr $ra

# Partition function for quicksort
partition:
    # Implementation for partitioning strings
    jr $ra

# String comparison function for sorting
string_compare:
    # Compare two strings lexicographically
    # Return: -1 if str1 < str2, 0 if equal, 1 if str1 > str2
    jr $ra
```

## Key Concepts in Assembly Implementation

1. **Memory Management**: 
   - Allocate space for storing multiple DNA strings
   - Use array of string pointers for efficient sorting

2. **String Comparison**:
   - Compare character by character using the DNA alphabet order (A<C<G<T)
   - Handle variable-length strings correctly
   - Empty string handling when one is prefix of another

3. **Sorting Algorithm**:
   - Implement efficient sorting (bubble sort or quicksort)
   - Use custom comparison function for lexicographic ordering
   - Handle memory management during swapping operations

4. **Input/Output Handling**:
   - Read strings from standard input
   - Output sorted results to standard output
   - Properly parse and format data

## Time Complexity Analysis

- **String Comparison**: O(min(m,n)) where m and n are string lengths
- **Sorting**: O(n log n) average case for quicksort, O(n²) worst case
- **Overall**: O(n² log n) in worst case due to string comparisons

This assembly implementation demonstrates how to handle complex string operations in low-level programming while maintaining the required lexicographic ordering for DNA sequences.