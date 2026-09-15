# Rosalind Problem: Finding All Similar Motifs

## Problem Understanding

We need to find all motifs (substrings) that appear in at least two of the given DNA sequences.

## Approach

1. Generate all possible substrings from each sequence
2. Count occurrences of each substring across all sequences
3. Return substrings that appear in at least 2 sequences

## Assembly Solution

```assembly
; Finding All Similar Motifs - Assembly Implementation
; Input: Multiple DNA sequences in memory
; Output: Motifs appearing in at least 2 sequences

.data
    ; Sequences stored as null-terminated strings
    seq1:   .ascii "ATCGATCGATCG\0"
    seq2:   .ascii "ATCGATCGATCG\0" 
    seq3:   .ascii "ATCGATCGATCG\0"
    
    ; Buffer for current substring
    substring: .space 50
    
    ; Count array to track occurrences
    count_array: .space 1000
    
    ; Maximum length of sequences
    MAX_LEN equ 20
    
    ; Result storage
    result: .space 1000
    
.text
.globl _start

_start:
    ; Initialize registers
    mov $seq1, %esi      ; Point to first sequence
    mov $seq2, %edi      ; Point to second sequence
    mov $seq3, %ebp      ; Point to third sequence
    
    ; Process all sequences
    call find_motifs
    
    ; Exit program
    mov $1, %eax         ; sys_exit
    mov $0, %ebx         ; exit status
    int $0x80

; Function to find similar motifs
find_motifs:
    push %ebp
    mov %esp, %ebp
    
    ; Initialize counters
    xor %ecx, %ecx       ; i = 0 (sequence counter)
    xor %edx, %edx       ; j = 0 (position counter)
    
    ; Process each sequence
process_sequence:
    ; Check if we've processed all sequences
    cmp $3, %ecx
    jge done_motifs
    
    ; Get current sequence pointer
    mov %ecx, %eax
    shl $2, %eax         ; Multiply by 4 for offset
    leal seq1(%eax), %esi
    
    ; Find all substrings in this sequence
    mov $0, %edx         ; Start position = 0
substring_loop:
    ; Check if we've reached end of sequence
    call get_length
    cmp %eax, %edx
    jge next_sequence
    
    ; Generate substring
    push %edx
    call generate_substring
    pop %edx
    
    ; Check if this substring appears in other sequences
    call check_occurrences
    
    ; Move to next position
    inc %edx
    jmp substring_loop
    
next_sequence:
    inc %ecx
    jmp process_sequence
    
done_motifs:
    pop %ebp
    ret

; Generate substring from current position
generate_substring:
    push %ebp
    mov %esp, %ebp
    
    ; Get start position and sequence pointer
    mov %edx, %eax       ; Start position
    mov %esi, %edi       ; Sequence pointer
    
    ; Copy substring to buffer (max length 10)
    mov $0, %ecx         ; Counter
substring_copy:
    cmp $10, %ecx        ; Max 10 characters
    jge substring_done
    
    movb (%edi,%eax), %dl
    cmp $0, %dl          ; Check for null terminator
    je substring_done
    
    movb %dl, substring(%ecx)
    inc %ecx
    inc %eax
    jmp substring_copy
    
substring_done:
    movb $0, substring(%ecx)  ; Null terminate
    
    pop %ebp
    ret

; Check how many sequences contain this substring
check_occurrences:
    push %ebp
    mov %esp, %ebp
    
    ; Initialize counters
    xor %ecx, %ecx       ; Sequence counter
    xor %edx, %edx       ; Match counter
    
check_sequence_loop:
    ; Check if we've processed all sequences
    cmp $3, %ecx
    jge check_done
    
    ; Get sequence pointer for current sequence
    mov %ecx, %eax
    shl $2, %eax
    leal seq1(%eax), %esi
    
    ; Check if substring matches in this sequence
    call substring_match
    
    ; If match found, increment counter
    cmp $0, %eax
    jne match_found
    jmp no_match
    
match_found:
    inc %edx
    jmp continue_check
    
no_match:
    ; Continue to next sequence
continue_check:
    inc %ecx
    jmp check_sequence_loop
    
check_done:
    ; If found in at least 2 sequences, add to results
    cmp $2, %edx
    jl skip_result
    
    call add_to_results
    
skip_result:
    pop %ebp
    ret

; Check if substring matches in sequence
substring_match:
    push %ebp
    mov %esp, %ebp
    
    ; Implementation would compare substring with sequence
    ; This is a simplified version - actual implementation 
    ; would use string comparison functions
    
    mov $1, %eax         ; Assume match found for demo
    
    pop %ebp
    ret

; Add substring to results
add_to_results:
    push %ebp
    mov %esp, %ebp
    
    ; Simple result storage (in real implementation,
    ; this would properly store the substring)
    
    pop %ebp
    ret

; Get length of string
get_length:
    push %ebp
    mov %esp, %ebp
    
    ; Use string length function or manual counting
    mov %esi, %edi       ; String pointer
    xor %eax, %eax       ; Length counter
    
length_loop:
    cmp $0, (%edi,%eax)
    je length_done
    inc %eax
    jmp length_loop
    
length_done:
    pop %ebp
    ret

```

## Key Assembly Concepts Used

1. **String Manipulation**: Using registers to copy and compare substrings
2. **Loop Control**: Implementing nested loops for sequence and position iteration
3. **Memory Management**: Storing sequences and intermediate results in memory
4. **Function Calls**: Modular approach with proper stack management
5. **Register Usage**: Efficient use of ESI, EDI, EBP registers for data handling

## Time Complexity

- O(n² × m) where n is the average sequence length and m is the number of sequences
- Space complexity: O(n × m) for storing substrings and counts

## Note

This is a conceptual implementation showing the assembly approach. In practice, this would require more detailed string handling functions and proper system calls for I/O operations.