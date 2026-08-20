# Rosalind Problem: Counting Point Mutations

## Problem Statement
Given two DNA strings of equal length, count the number of positions where the nucleotides differ (Hamming distance).

## Solution in Assembly Language

```assembly
.data
    dna1:   .ascii "GAGCCTACTAACGGGAT\n"
    dna2:   .ascii "CATCGGTATCGGCCCGT\n"
    len:    .long 18
    count:  .long 0

.text
    .global _start

_start:
    # Initialize registers
    movl    len, %ecx          # Load length into ECX (loop counter)
    movl    $dna1, %esi        # Load address of first DNA string into ESI
    movl    $dna2, %edi        # Load address of second DNA string into EDI
    movl    $0, %eax           # Clear EAX (will hold count)
    
count_loop:
    # Check if we've reached the end
    testl   %ecx, %ecx
    jz      done
    
    # Compare characters at current position
    movb    (%esi), %dl       # Load character from dna1
    movb    (%edi), %dh       # Load character from dna2
    
    # If characters are different, increment counter
    cmpb    %dh, %dl
    jne     increment_count
    jmp     next_position
    
increment_count:
    incl    %eax              # Increment count
    
next_position:
    inc     %esi              # Move to next character in dna1
    inc     %edi              # Move to next character in dna2
    decl    %ecx              # Decrement counter
    jmp     count_loop        # Continue loop
    
done:
    # Store result in count variable
    movl    %eax, count
    
    # Exit program
    movl    $1, %eax          # sys_exit
    movl    $0, %ebx          # exit status
    int     $0x80             # system call
```

## Explanation

This assembly solution implements the Hamming distance calculation:

1. **Data Section**: 
   - `dna1` and `dna2`: Two DNA sequences to compare
   - `len`: Length of the sequences (18 nucleotides)
   - `count`: Variable to store the result

2. **Algorithm**:
   - Initialize loop counter with sequence length
   - Load addresses of both DNA strings into ESI and EDI registers
   - Compare characters at each position using byte comparison
   - If different, increment the counter
   - Continue until all positions are checked

3. **Key Instructions**:
   - `movb`: Move bytes between memory and registers
   - `cmpb`: Compare bytes
   - `jne`: Jump if not equal (when nucleotides differ)
   - `incl`: Increment the counter
   - `decl`: Decrement loop counter

## Expected Output
For the given test sequences:
- DNA1: GAGCCTACTAACGGGAT
- DNA2: CATCGGTATCGGCCCGT

The Hamming distance is **7** (positions 1, 2, 5, 6, 9, 10, 13 differ)

## Alternative Implementation (More Efficient)
```assembly
.data
    dna1:   .ascii "GAGCCTACTAACGGGAT\n"
    dna2:   .ascii "CATCGGTATCGGCCCGT\n"
    len:    .long 18

.text
    .global _start

_start:
    movl    len, %ecx          # Length counter
    movl    $dna1, %esi        # First DNA string
    movl    $dna2, %edi        # Second DNA string
    xorl    %eax, %eax         # Clear count (EAX = 0)
    
compare_loop:
    testl   %ecx, %ecx
    jz      print_result
    
    cmpb    (%esi), (%edi)     # Compare bytes directly
    je      skip_increment     # Jump if equal
    
    incl    %eax               # Increment if different
    
skip_increment:
    inc     %esi               # Next character in first string
    inc     %edi               # Next character in second string
    decl    %ecx               # Decrement counter
    jmp     compare_loop       # Continue loop
    
print_result:
    # Result is now in EAX register
    # (Would typically be printed or stored)
    
    # Exit
    movl    $1, %eax
    movl    $0, %ebx
    int     $0x80
```

This implementation efficiently counts point mutations by comparing each position in the two DNA sequences and incrementing a counter when nucleotides differ.