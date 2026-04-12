# Rosalind Problem: Constructing a De Bruijn Graph

## Problem Understanding

In bioinformatics, a De Bruijn graph is a directed graph used for genome assembly. Given a set of DNA sequences (reads), we need to construct a De Bruijn graph where:
- Vertices represent (k-1)-mers (overlapping prefixes/suffixes)
- Edges represent overlaps between k-mers

## Solution Approach

I'll implement this in Assembly language using a straightforward approach:

```assembly
; De Bruijn Graph Construction
; Input: Set of DNA sequences
; Output: De Bruijn graph as adjacency list

.data
    ; Input sequences (example)
    sequences: .ascii "ACGTACGTA"
    seq_len: .long 9
    k: .long 3
    
    ; Graph storage
    vertices: .space 1000
    edges: .space 1000
    vertex_count: .long 0
    edge_count: .long 0
    
    ; Temporary buffers
    prefix: .space 10
    suffix: .space 10
    
.text
.globl _start

_start:
    ; Initialize
    movl $0, %eax          ; vertex_count = 0
    movl $0, %ebx          ; edge_count = 0
    
    ; Process each sequence
    movl sequences, %ecx   ; sequence pointer
    movl seq_len, %edx     ; sequence length
    
    ; Loop through sequence to extract k-mers
    movl $0, %esi          ; position counter
    
extract_kmers:
    cmpb $0, (%ecx)        ; check if end of string
    je graph_done
    
    ; Extract k-mer
    movl %esi, %edi        ; position
    addl $k, %edi          ; end position
    cmpb $0, (%edi)        ; check if k-mer fits
    jg extract_kmers       ; if not, skip
    
    ; Extract prefix (k-1) and suffix (k-1)
    movl %esi, %edi
    addl $1, %edi          ; start from position + 1
    
    ; Copy prefix (k-1) characters
    movl $0, %edi
    movl %esi, %ebp
    addl $k, %ebp          ; end position of k-mer
    
prefix_loop:
    cmpb $0, (%ecx)        ; check if end of string
    je prefix_done
    cmpb %ebp, %edi        ; check if we've copied k-1 chars
    je prefix_done
    movb (%ecx), %al       ; copy character
    movb %al, prefix(%edi) ; store in prefix
    incb %edi
    incb %ecx
    jmp prefix_loop
    
prefix_done:
    movb $0, prefix(%edi)  ; null terminate
    
    ; Extract suffix
    movl %esi, %edi
    addl $k, %edi          ; end position
    subl $1, %edi          ; start from k-1 position
    
suffix_loop:
    cmpb $0, (%ecx)        ; check if end of string
    je suffix_done
    cmpb %edi, %esi        ; check if we've copied k-1 chars
    je suffix_done
    movb (%ecx), %al       ; copy character
    movb %al, suffix(%edi) ; store in suffix
    decb %edi
    decb %ecx
    jmp suffix_loop
    
suffix_done:
    movb $0, suffix(%edi)  ; null terminate
    
    ; Add vertices to graph
    call add_vertex
    call add_vertex
    
    ; Add edge
    call add_edge
    
    ; Move to next k-mer
    incb %esi
    jmp extract_kmers

; Function to add vertex to graph
add_vertex:
    ; Input: prefix or suffix string in buffer
    ; Output: vertex added to graph
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    ; Check if vertex already exists
    call vertex_exists
    
    ; If not exists, add it
    cmpl $0, %eax
    jne vertex_already_exists
    
    ; Add new vertex
    movl vertex_count, %eax
    movl %eax, vertices(%eax)  ; store vertex
    incl vertex_count
    
vertex_already_exists:
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

; Function to check if vertex exists
vertex_exists:
    ; Input: vertex string in prefix/suffix buffer
    ; Output: 0 if exists, 1 if doesn't exist
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $0, %eax          ; assume doesn't exist
    movl $0, %ebx          ; vertex index
    
check_loop:
    cmpl vertex_count, %ebx
    jge vertex_not_found
    
    ; Compare vertex at index with input
    call compare_strings
    cmpl $0, %eax          ; if strings equal
    je vertex_found
    
    incl %ebx
    jmp check_loop
    
vertex_not_found:
    movl $1, %eax          ; doesn't exist
    jmp vertex_end
    
vertex_found:
    movl $0, %eax          ; exists
    
vertex_end:
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

; Function to add edge
add_edge:
    ; Input: prefix and suffix strings
    ; Output: edge added to graph
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    ; Create edge from prefix to suffix
    movl edge_count, %eax
    movl %eax, edges(%eax)  ; store edge
    incl edge_count
    
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

; Function to compare strings
compare_strings:
    ; Input: two strings in memory
    ; Output: 0 if equal, 1 if different
    pushl %eax
    pushl %ebx
    pushl %ecx
    pushl %edx
    
    movl $0, %eax          ; assume equal
    
    ; Compare character by character
    movl %esi, %edi        ; first string
    movl %edi, %ebx        ; second string
    
compare_loop:
    cmpb $0, (%edi)        ; check if end of first string
    je compare_end
    
    cmpb $0, (%ebx)        ; check if end of second string
    je compare_end
    
    cmpb (%edi), (%ebx)    ; compare characters
    jne strings_different
    
    incb %edi
    incb %ebx
    jmp compare_loop
    
strings_different:
    movl $1, %eax          ; strings different
    
compare_end:
    popl %edx
    popl %ecx
    popl %ebx
    popl %eax
    ret

graph_done:
    ; Print graph
    movl $1, %eax          ; sys_exit
    movl $0, %ebx          ; exit status
    int $0x80
```

## Algorithm Explanation

1. **Input Processing**: Read DNA sequences from input
2. **K-mer Extraction**: Extract all k-mers from each sequence
3. **Prefix/Suffix Extraction**: For each k-mer, extract (k-1) prefix and suffix
4. **Vertex Creation**: Add unique (k-1)-mers as vertices
5. **Edge Creation**: Connect vertices with directed edges based on overlaps
6. **Graph Output**: Return adjacency list representation of De Bruijn graph

## Time Complexity
- O(n × k) where n is the total length of all sequences and k is the k-mer size

## Space Complexity  
- O(n × k) for storing vertices and edges

This assembly implementation provides the basic framework for constructing De Bruijn graphs, though a full implementation would require additional helper functions for string manipulation and memory management.

