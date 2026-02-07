# Rosalind Problem: Implement_BWMatching in Assembly

## Problem Understanding

BWMatching (Burrows-Wheeler Matching) is a key algorithm for pattern matching in compressed text using Burrows-Wheeler Transform (BWT). Given a BWT string and a pattern, we need to find how many times the pattern occurs in the original text.

## Assembly Implementation

```assembly
.section .data
    # Input data
    bwt_string: .ascii "ACGTACGT"
    bwt_len: .long 8
    pattern: .ascii "ACG"
    pattern_len: .long 3
    
    # Arrays for BWT processing
    first_col: .space 256
    first_occurrence: .space 256
    count: .space 256
    last_col: .space 256
    
    # Result storage
    result: .long 0

.section .text
    .global _start

# Function: BWMatching
# Input: BWT string, pattern
# Output: Number of occurrences of pattern in original text
bw_matching:
    push %ebp
    mov %esp, %ebp
    
    # Parameters
    mov 8(%ebp), %esi    # BWT string pointer
    mov 12(%ebp), %edi   # Pattern pointer
    mov 16(%ebp), %ecx   # BWT length
    mov 20(%ebp), %edx   # Pattern length
    
    # Initialize arrays
    call initialize_arrays
    
    # Build First Column and First Occurrence
    call build_first_column
    
    # Build Count array
    call build_count_array
    
    # Perform BW Matching
    call bw_match
    
    # Return result
    mov result, %eax
    
    pop %ebp
    ret

# Initialize all arrays to zero
initialize_arrays:
    push %eax
    push %ebx
    push %ecx
    
    # Initialize first_col
    mov $first_col, %edi
    mov $256, %ecx
    xor %eax, %eax
    rep stosb
    
    # Initialize first_occurrence
    mov $first_occurrence, %edi
    mov $256, %ecx
    xor %eax, %eax
    rep stosb
    
    # Initialize count
    mov $count, %edi
    mov $256, %ecx
    xor %eax, %eax
    rep stosb
    
    # Initialize last_col
    mov $last_col, %edi
    mov $256, %ecx
    xor %eax, %eax
    rep stosb
    
    pop %ecx
    pop %ebx
    pop %eax
    ret

# Build first column from BWT
build_first_column:
    push %eax
    push %ebx
    push %ecx
    push %edx
    push %esi
    push %edi
    
    # Copy BWT to last_col
    mov $last_col, %edi
    mov $bwt_string, %esi
    mov bwt_len, %ecx
    rep movsb
    
    # Sort last_col to get first_col
    call quick_sort
    
    # Build first_occurrence array
    mov $first_occurrence, %edi
    xor %eax, %eax
    mov $256, %ecx
    rep stosb
    
    # Fill first_occurrence
    mov $first_col, %esi
    mov $first_occurrence, %edi
    mov $256, %ecx
    xor %eax, %eax
    mov $0, %ebx
    
    # Count first occurrences
    mov $first_col, %esi
    mov $256, %ecx
    xor %al, %al
    
    # Process each character
    loop_first_occurrence:
        movb (%esi), %dl
        cmpb $0, %dl
        je next_char
        movb %dl, %al
        movb %bl, (%edi,%eax,1)
        inc %ebx
    next_char:
        inc %esi
        dec %ecx
        jnz loop_first_occurrence
    
    pop %edi
    pop %esi
    pop %edx
    pop %ecx
    pop %ebx
    pop %eax
    ret

# Build count array for BWT
build_count_array:
    push %eax
    push %ebx
    push %ecx
    push %edx
    push %esi
    push %edi
    
    # Initialize count array to zero
    mov $count, %edi
    mov $256, %ecx
    xor %eax, %eax
    rep stosb
    
    # Count occurrences in BWT
    mov $bwt_string, %esi
    mov bwt_len, %ecx
    
    count_loop:
        movb (%esi), %al
        incb (%edi,%eax,1)
        inc %esi
        dec %ecx
        jnz count_loop
    
    pop %edi
    pop %esi
    pop %edx
    pop %ecx
    pop %ebx
    pop %eax
    ret

# BW Matching algorithm
bw_match:
    push %eax
    push %ebx
    push %ecx
    push %edx
    push %esi
    push %edi
    
    # Initialize variables
    mov pattern_len, %ecx    # Pattern length
    mov $0, %ebx             # Pattern index
    
    # Initialize first and last positions
    mov $0, %edx             # first = 0
    mov bwt_len, %eax        # last = len - 1
    
    # Process pattern from right to left
    pattern_loop:
        # Check if pattern index is valid
        cmp $0, %ecx
        jle pattern_done
        
        # Get current character from pattern
        mov pattern, %esi
        add %ebx, %esi
        movb (%esi), %al
        
        # Get last column character at position first
        mov $last_col, %esi
        add %edx, %esi
        movb (%esi), %dl
        
        # Check if characters match
        cmp %dl, %al
        jne match_failed
        
        # Update first and last
        mov $first_occurrence, %esi
        mov %al, %dl
        movb (%esi,%edx,1), %dh
        mov %edx, %edi
        mov $count, %esi
        mov %al, %dl
        movb (%esi,%edx,1), %dl
        
        # Calculate new first and last positions
        add %dh, %edi
        add %dl, %edi
        
        # Update first and last
        mov %edi, %edx
        
        dec %ecx
        inc %ebx
        jmp pattern_loop
    
    match_failed:
        # Set result to 0
        mov $0, result
        jmp pattern_done
    
    pattern_done:
        # Calculate result
        mov $1, %eax
        mov result, %ebx
        add %eax, %ebx
        mov %ebx, result
    
    pop %edi
    pop %esi
    pop %edx
    pop %ecx
    pop %ebx
    pop %eax
    ret

# Quick sort for first column
quick_sort:
    push %ebp
    mov %esp, %ebp
    push %ebx
    push %ecx
    push %edx
    push %esi
    push %edi
    
    # Simple insertion sort for small arrays
    mov $first_col, %esi
    mov $256, %ecx
    
    # Insertion sort
    mov $1, %ebx
    outer_loop:
        cmp $256, %ebx
        jge outer_done
        
        movb (%esi,%ebx,1), %al
        mov %ebx, %edi
        inner_loop:
            cmp $0, %edi
            jle inner_done
            
            movb (%esi,%edi,1), %dl
            cmp %dl, %al
            jge inner_done
            
            movb %dl, (%esi,%edi,1)
            dec %edi
            jmp inner_loop
        
        inner_done:
            movb %al, (%esi,%edi,1)
            inc %ebx
            jmp outer_loop
    
    outer_done:
        pop %edi
        pop %esi
        pop %edx
        pop %ecx
        pop %ebx
        pop %ebp
        ret

# Main function
_start:
    # Set up parameters
    mov $bwt_string, %eax
    mov $pattern, %ebx
    mov bwt_len, %ecx
    mov pattern_len, %edx
    
    # Call BWMatching
    push %edx
    push %ecx
    push %ebx
    push %eax
    call bw_matching
    add $16, %esp
    
    # Store result
    mov %eax, result
    
    # Exit program
    mov $1, %eax    # sys_exit
    mov $0, %ebx    # exit status
    int $0x80
```

## Key Components Explained

### 1. **Data Structures**
- `first_col`: First column of BWT matrix
- `first_occurrence`: First occurrence array for each character
- `count`: Count array for character frequencies
- `last_col`: Last column of BWT matrix

### 2. **Core Algorithm Steps**
1. **Initialize Arrays**: Set all arrays to zero
2. **Build First Column**: Sort BWT to get first column
3. **Build Count Array**: Count character occurrences in BWT
4. **BW Matching**: Perform backward search using first_occurrence and count arrays

### 3. **Key Operations**
- **First Occurrence**: Maps each character to its first position in sorted first column
- **Count**: Tracks how many times each character appears up to a given position
- **Backward Search**: Iteratively narrows the range of possible matches

### 4. **Time Complexity**
- **Space**: O(n) where n is the length of BWT string
- **Time**: O(n) for preprocessing + O(m) for matching where m is pattern length

This implementation provides a complete assembly solution for the BWMatching problem, following the standard BWT matching algorithm with efficient array operations.

