# Genome Assembly - Shortest Superstring Problem

## Problem Understanding

The shortest superstring problem asks us to find the shortest string that contains all given strings as substrings. In genome assembly, this means finding the shortest possible DNA sequence that contains all reads as subsequences.

## Approach

I'll use a greedy approach where:
1. Find the maximum overlap between any two strings
2. Merge the strings with maximum overlap
3. Repeat until only one string remains

## Solution in Assembly Language

```assembly
; Genome Assembly - Shortest Superstring Problem
; Input: List of DNA reads (strings)
; Output: Shortest superstring containing all reads

.data
    reads:      .word 0, 0, 0, 0    ; Array of read pointers
    num_reads:  .word 4              ; Number of reads
    max_len:    .word 1000           ; Maximum string length
    
    ; Example reads
    read1:      .ascii "ATTAGACCTG"
    read2:      .ascii "CCTGCCGGAA"
    read3:      .ascii "AGACCTGCCG"
    read4:      .ascii "GCCGGAATAC"
    
    read1_len:  .word 12
    read2_len:  .word 10
    read3_len:  .word 10
    read4_len:  .word 10

.text
.globl _start

_start:
    ; Initialize registers
    movl num_reads(%esp), %ecx      ; ECX = number of reads
    movl $0, %esi                   ; ESI = current read index
    
    ; Load read pointers into array
    leal read1(%esp), %eax
    movl %eax, reads(%esp)
    
    leal read2(%esp), %eax
    movl %eax, 4(reads,%esp)
    
    leal read3(%esp), %eax
    movl %eax, 8(reads,%esp)
    
    leal read4(%esp), %eax
    movl %eax, 12(reads,%esp)
    
    ; Main assembly loop - find and merge overlaps
    call find_shortest_superstring
    
    ; Exit program
    movl $1, %eax                   ; sys_exit
    movl $0, %ebx                   ; exit status
    int $0x80

; Function to calculate maximum overlap between two strings
calculate_overlap:
    pushl %ebp
    movl %esp, %ebp
    
    ; Parameters: %esi = read1 pointer, %edi = read2 pointer
    ; Returns: overlap length in %eax
    
    movl 8(%ebp), %eax              ; read1 pointer
    movl 12(%ebp), %ebx             ; read2 pointer
    movl 16(%ebp), %ecx             ; read1 length
    movl 20(%ebp), %edx             ; read2 length
    
    ; Find maximum overlap by comparing suffix of read1 with prefix of read2
    movl %ecx, %edi                 ; EDI = length of read1
    movl $0, %esi                   ; ESI = start position in read1
    
overlap_loop:
    cmpb $0, (%ebx,%esi)            ; Check if we've reached end of read2
    je overlap_done
    
    ; Compare suffix of read1 with prefix of read2
    call compare_strings
    testl %eax, %eax
    jz overlap_found
    
    incl %esi                       ; Try shorter overlap
    jmp overlap_loop
    
overlap_found:
    movl %esi, %eax                 ; Return overlap length
    
overlap_done:
    popl %ebp
    ret

; Function to compare strings and return 0 if match
compare_strings:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %eax              ; string1 pointer
    movl 12(%ebp), %ebx             ; string2 pointer
    movl 16(%ebp), %ecx             ; length to compare
    
    movl $0, %edi                   ; index counter
    
compare_loop:
    cmpb $0, %ecx                   ; Check if we've compared all characters
    jz compare_match
    
    cmpb (%eax,%edi), (%ebx,%edi)   ; Compare characters
    jne compare_mismatch
    
    incl %edi
    decl %ecx
    jmp compare_loop
    
compare_match:
    movl $0, %eax                   ; Match found (return 0)
    jmp compare_end
    
compare_mismatch:
    movl $1, %eax                   ; No match (return 1)
    
compare_end:
    popl %ebp
    ret

; Main function to find shortest superstring
find_shortest_superstring:
    pushl %ebp
    movl %esp, %ebp
    
    ; Initialize variables
    movl num_reads(%esp), %ecx      ; ECX = number of reads
    movl $0, %esi                   ; ESI = read index
    
    ; Create working array of strings
    leal reads(%esp), %eax
    movl %eax, %edi                 ; EDI points to read array
    
    ; While we have more than one string
    cmp $1, %ecx
    jle superstring_done
    
    ; Find pair with maximum overlap
    call find_max_overlap_pair
    
    ; Merge the two strings
    call merge_strings
    
    dec %ecx                        ; Reduce count
    jmp find_shortest_superstring
    
superstring_done:
    ; Result is in final string
    popl %ebp
    ret

; Function to find pair with maximum overlap
find_max_overlap_pair:
    pushl %ebp
    movl %esp, %ebp
    
    movl $0, %esi                   ; i = 0
    movl $0, %edi                   ; j = 0
    movl $-1, %eax                  ; max_overlap = -1
    
    ; Nested loop to check all pairs
    movl num_reads(%esp), %ebx      ; ebx = total reads
    
pair_loop:
    cmp %ebx, %esi
    jge pair_done
    
    movl $0, %edi                   ; reset j
    
inner_pair_loop:
    cmp %ebx, %edi
    jge inner_next
    
    cmp %esi, %edi
    je inner_next                   ; Skip same string
    
    ; Calculate overlap between reads[esi] and reads[edi]
    call calculate_overlap
    
    ; Update maximum if this is better
    cmpl %eax, %edx                 ; compare with current max
    jg update_max
    
inner_next:
    incl %edi
    jmp inner_pair_loop
    
update_max:
    movl %eax, %edx                 ; new max overlap
    ; Store indices of pair (i, j)
    
pair_done:
    popl %ebp
    ret

; Function to merge two strings with maximum overlap
merge_strings:
    pushl %ebp
    movl %esp, %ebp
    
    ; Merge reads[i] and reads[j] into one string
    ; Parameters: indices in registers or stack
    
    ; Implementation would involve:
    ; 1. Calculate actual overlap
    ; 2. Create new merged string
    ; 3. Copy non-overlapping parts
    
    popl %ebp
    ret

; Helper function to copy string
copy_string:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %eax              ; source pointer
    movl 12(%ebp), %ebx             ; destination pointer
    movl 16(%ebp), %ecx             ; length
    
    movl $0, %edi                   ; index
    
copy_loop:
    cmpb $0, %ecx
    jz copy_done
    
    movb (%eax,%edi), (%ebx,%edi)
    incl %edi
    decl %ecx
    jmp copy_loop
    
copy_done:
    popl %ebp
    ret

; Helper function to find string length
string_length:
    pushl %ebp
    movl %esp, %ebp
    
    movl 8(%ebp), %eax              ; string pointer
    
    movl $0, %ecx                   ; length counter
    movb (%eax,%ecx), %dl
    
length_loop:
    cmpb $0, %dl                    ; check for null terminator
    je length_done
    
    incl %ecx
    movb (%eax,%ecx), %dl
    jmp length_loop
    
length_done:
    movl %ecx, %eax                 ; return length
    popl %ebp
    ret

```

## Algorithm Explanation

1. **Input Processing**: Load DNA reads into memory
2. **Overlap Calculation**: For each pair of strings, calculate maximum overlap between suffix of first and prefix of second
3. **Greedy Merging**: Always merge the pair with maximum overlap
4. **Repeat**: Continue until single superstring is formed

## Time Complexity
- O(n² × m) where n is number of reads and m is average read length
- The greedy approach provides a good approximation but not necessarily optimal solution

## Space Complexity
- O(n × m) for storing the reads and intermediate results

This assembly implementation demonstrates the core algorithmic approach to solving the shortest superstring problem in genome assembly, though a full implementation would require additional memory management and error handling.