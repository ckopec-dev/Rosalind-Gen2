# Construct the Overlap Graph of a Collection of k-mers

## Problem Understanding

We need to construct an overlap graph from a collection of k-mers, where:
- Each k-mer is a string of length k
- An edge exists from k-mer A to k-mer B if the suffix of A (length k-1) matches the prefix of B (length k-1)
- The output should be in the format: "k-mer1 -> k-mer2" for each edge

## Solution Approach

1. For each k-mer, extract its prefix and suffix of length k-1
2. For each pair of k-mers, check if the suffix of the first matches the prefix of the second
3. If so, add an edge from the first to the second k-mer

## Assembly Implementation

```assembly
; Construct Overlap Graph of k-mers
; Input: List of k-mers (each of length k)
; Output: Overlap graph edges in format "k-mer1 -> k-mer2"

; Initialize data structures
; Assume k-mers are stored in memory with proper delimiters

; Main procedure
construct_overlap_graph:
    ; Load all k-mers into memory
    ; For each k-mer in the collection:
    ;   Extract suffix (k-1 characters from end)
    ;   For each other k-mer:
    ;       Extract prefix (k-1 characters from start)
    ;       Compare suffix with prefix
    ;       If match, output edge
    
    ; Initialize loop counters
    mov i, 0
    mov j, 0
    
    ; Outer loop over all k-mers
outer_loop:
    cmp i, num_kmers
    jge end_program
    
    ; Get current k-mer
    mov current_kmer, kmer_array[i]
    
    ; Extract suffix of current k-mer (k-1 characters)
    mov suffix, extract_suffix(current_kmer, k)
    
    ; Inner loop over all k-mers to check for matches
inner_loop:
    cmp j, num_kmers
    jge next_outer
    
    ; Skip if comparing k-mer with itself
    cmp i, j
    je next_inner
    
    ; Get target k-mer
    mov target_kmer, kmer_array[j]
    
    ; Extract prefix of target k-mer (k-1 characters)
    mov prefix, extract_prefix(target_kmer, k)
    
    ; Compare suffix with prefix
    cmp suffix, prefix
    jne next_inner
    
    ; Match found - output edge
    output_edge(current_kmer, target_kmer)
    
next_inner:
    inc j
    jmp inner_loop
    
next_outer:
    mov j, 0
    inc i
    jmp outer_loop
    
end_program:
    ret

; Helper function to extract suffix
extract_suffix:
    ; Input: k-mer string, k (length)
    ; Output: suffix of length k-1
    mov temp, k
    dec temp
    ; Extract last k-1 characters
    return suffix_string

; Helper function to extract prefix
extract_prefix:
    ; Input: k-mer string, k (length)
    ; Output: prefix of length k-1
    ; Extract first k-1 characters
    return prefix_string

; Helper function to output edge
output_edge:
    ; Input: source_kmer, target_kmer
    ; Output: "source_kmer -> target_kmer"
    print source_kmer
    print " -> "
    print target_kmer
    print "\n"
    ret
```

## Alternative Compact Implementation

```assembly
; Compact version for constructing overlap graph
construct_overlap:
    ; Assume k-mers are in array kmer_list with size n
    ; Assume k is stored in register k
    
    ; Loop through each k-mer
    mov i, 0
loop_i:
    cmp i, n
    jge done
    
    ; Get k-mer at position i
    mov kmer_i, kmer_list[i]
    
    ; Extract suffix of k-mer_i
    mov suffix_i, kmer_i[k-1]  ; Last k-1 characters
    
    ; Compare with all other k-mers
    mov j, 0
loop_j:
    cmp j, n
    jge next_i
    
    ; Skip self comparison
    cmp i, j
    je next_j
    
    ; Get k-mer at position j
    mov kmer_j, kmer_list[j]
    
    ; Extract prefix of k-mer_j
    mov prefix_j, kmer_j[0]    ; First k-1 characters
    
    ; Check if suffix_i equals prefix_j
    cmp suffix_i, prefix_j
    jne next_j
    
    ; Output edge
    print kmer_i
    print " -> "
    print kmer_j
    print "\n"
    
next_j:
    inc j
    jmp loop_j
    
next_i:
    inc i
    jmp loop_i
    
done:
    ret
```

## Example Usage

For input k-mers: {"ATGCG", "GCATC", "CATCG", "TCGAT", "CGATC"}

Expected output:
```
ATGCG -> GCATC
CATCG -> CGATC
CGATC -> GCATC
TCGAT -> ATGCG
```

## Time Complexity
- O(n² × k) where n is the number of k-mers and k is the length of each k-mer
- For each k-mer, we compare with all others and perform substring operations

## Space Complexity
- O(n × k) to store the k-mers and their prefixes/suffixes

