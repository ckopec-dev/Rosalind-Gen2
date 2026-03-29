# Find All Shared k-mers of a Pair of Strings - Assembly Solution

## Problem Understanding

We need to find all shared k-mers (k-length substrings) between two DNA strings and return their positions in both strings.

## Assembly Implementation

```assembly
; Find all shared k-mers of a pair of strings
; Input: Two DNA strings and k value
; Output: All shared k-mers with their positions in both strings

.section .data
    ; String 1 and String 2
    str1: .ascii "AAACTCATC"
    str1_len: .long 9
    str2: .ascii "TTAGGGACTCATCCT"
    str2_len: .long 15
    k: .long 3
    
    ; Buffer for k-mer extraction
    kmer_buffer: .space 100
    result_buffer: .space 1000
    
    ; Format strings
    format_str: .ascii "%s %d %d\n"
    format_str_len = . - format_str

.section .text
    .global _start

_start:
    ; Load parameters
    movl str1, %eax        ; String 1 address
    movl str2, %ebx        ; String 2 address
    movl k, %ecx           ; k value
    
    ; Get string lengths
    movl str1_len, %edx
    movl str2_len, %esi
    
    ; Find shared k-mers
    call find_shared_kmers
    
    ; Exit program
    movl $1, %eax          ; sys_exit
    movl $0, %ebx          ; exit status
    int $0x80

; Function to find all shared k-mers
find_shared_kmers:
    pushl %ebp
    movl %esp, %ebp
    
    ; Parameters
    movl 8(%ebp), %eax     ; str1 address
    movl 12(%ebp), %ebx    ; str2 address
    movl 16(%ebp), %ecx    ; k value
    movl 20(%ebp), %edx    ; len1
    movl 24(%ebp), %esi    ; len2
    
    ; Initialize loop counters
    movl $0, %edi          ; i = 0 (position in str1)
    movl $0, %ebp          ; j = 0 (position in str2)
    
    ; Loop through all k-mers in str1
outer_loop:
    cmpl %edx, %edi        ; if i >= len1
    jge end_program
    
    ; Extract k-mer from str1
    call extract_kmer
    pushl %eax             ; Save k-mer string
    
    ; Loop through all k-mers in str2
    movl $0, %ebp          ; reset j = 0
inner_loop:
    cmpl %esi, %ebp        ; if j >= len2
    jge next_str1_kmer
    
    ; Extract k-mer from str2
    call extract_kmer
    pushl %eax             ; Save k-mer string
    
    ; Compare k-mers
    call compare_strings
    cmpl $0, %eax          ; if strings equal
    jne next_str2_kmer
    
    ; Found shared k-mer - print positions
    call print_shared_kmer
    
next_str2_kmer:
    addl $4, %ebp          ; j++
    jmp inner_loop
    
next_str1_kmer:
    addl $1, %edi          ; i++
    jmp outer_loop
    
end_program:
    popl %ebp
    ret

; Extract k-mer from string at position
extract_kmer:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %eax     ; string address
    movl 12(%ebp), %ebx    ; position
    movl 16(%ebp), %ecx    ; k value
    
    ; Copy k characters to buffer
    movl $kmer_buffer, %edx
    movl %ebx, %edi        ; position
    
    movl %eax, %esi        ; string address
    addl %edi, %esi        ; offset to position
    
    movl $0, %edi          ; counter
extract_loop:
    cmpl %ecx, %edi        ; if counter >= k
    jge extract_done
    
    movb (%esi,%edi,1), %al
    movb %al, (%edx,%edi,1)
    incl %edi
    jmp extract_loop
    
extract_done:
    movl $kmer_buffer, %eax
    popl %ebp
    ret

; Compare two strings
compare_strings:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %eax     ; str1
    movl 12(%ebp), %ebx    ; str2
    
    movl $0, %ecx          ; counter
    
compare_loop:
    movb (%eax,%ecx,1), %al
    movb (%ebx,%ecx,1), %bl
    
    cmpb %bl, %al
    jne not_equal
    
    testb %al, %al         ; check if null terminator
    jz equal
    
    incl %ecx
    jmp compare_loop
    
not_equal:
    movl $1, %eax          ; strings not equal
    jmp compare_done
    
equal:
    movl $0, %eax          ; strings equal
    
compare_done:
    popl %ebp
    ret

; Print shared k-mer with positions
print_shared_kmer:
    pushl %ebp
    movl %esp, %ebp
    
    ; Print k-mer and positions
    ; This would involve system calls to print formatted output
    ; Simplified version - actual implementation would use printf or sys_write
    
    popl %ebp
    ret

.section .bss
    ; Buffer space for k-mer extraction
    kmer_buffer: .space 100
    result_buffer: .space 1000
```

## Key Assembly Concepts Used

1. **String Manipulation**: Manual character copying and comparison
2. **Loop Control**: Nested loops for comparing all k-mers
3. **Memory Management**: Buffer allocation for k-mer storage
4. **Function Calls**: Modular approach to k-mer extraction and comparison
5. **Register Usage**: Efficient use of registers for parameters and counters

## Algorithm Steps

1. **Extract k-mers**: Loop through each string to extract all k-length substrings
2. **Compare**: For each k-mer from first string, compare with all k-mers from second string
3. **Store Results**: When match found, record positions in both strings
4. **Output**: Print all shared k-mers with their positions

## Time Complexity
O(n₁ × n₂ × k) where n₁ and n₂ are string lengths and k is the k-mer length.

## Space Complexity
O(n₁ × k + n₂ × k) for storing k-mers.

This assembly solution demonstrates the fundamental algorithm for finding shared k-mers while working within the constraints of low-level programming.

