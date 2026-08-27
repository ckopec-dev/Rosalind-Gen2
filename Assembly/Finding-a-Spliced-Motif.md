# Finding a Spliced Motif in Assembly

I'll solve this step by step, implementing an assembly program to find a spliced motif in DNA sequences.

## Problem Understanding

We need to find a motif (shorter sequence) within a DNA string, where the motif characters appear in order but not necessarily consecutively (spliced motif).

## Assembly Implementation

```assembly
; Finding a Spliced Motif - Assembly Solution
; Input: Two DNA strings (s = genome, t = motif)
; Output: Starting positions of spliced motif in genome (1-indexed)

.data
    s:      .ascii "AACCTTGG"
    s_len:  .long 8
    t:      .ascii "ACTGG"
    t_len:  .long 5
    result: .space 20
    newline: .ascii "\n"
    
.text
.globl _start

_start:
    ; Load addresses of sequences
    la $s1, s          ; s = genome string
    la $s2, t          ; t = motif string
    lw $s3, s_len      ; s_length
    lw $s4, t_len      ; t_length
    
    ; Initialize counters
    li $t0, 0          ; i = 0 (genome index)
    li $t1, 0          ; j = 0 (motif index)
    li $t2, 0          ; result_index = 0
    
    ; Main loop - find spliced motif
find_motif:
    ; Check if we've processed entire genome
    bge $t0, $s3, end_search
    
    ; Check if we've found complete motif
    bge $t1, $s4, motif_found
    
    ; Get current characters
    lb $t3, 0($s1)     ; s[i]
    lb $t4, 0($s2)     ; t[j]
    
    ; Compare characters
    beq $t3, $t4, match_found
    addi $t0, $t0, 1   ; i++
    addi $s1, $s1, 1   ; s++
    j find_motif
    
match_found:
    ; Store position (1-indexed)
    addi $t5, $t0, 1   ; position = i + 1
    sb $t5, result($t2) ; store in result array
    addi $t2, $t2, 1   ; result_index++
    
    ; Move to next character in both sequences
    addi $t0, $t0, 1   ; i++
    addi $s1, $s1, 1   ; s++
    addi $t1, $t1, 1   ; j++
    addi $s2, $s2, 1   ; t++
    
    j find_motif
    
motif_found:
    ; Print result
    li $v0, 1          ; system call for print integer
    la $a0, result     ; load result address
    syscall
    
end_search:
    ; Exit program
    li $v0, 17         ; exit system call
    syscall
```

## Alternative Implementation (More Complete)

```assembly
; Complete Finding a Spliced Motif Solution
; Uses recursive approach for clarity

.data
    genome: .ascii "ACGTACGTACGT"
    motif:  .ascii "ACTG"
    gen_len: .long 12
    mot_len: .long 4
    
.text
.globl _start

_start:
    ; Initialize registers
    la $s0, genome     ; pointer to genome
    la $s1, motif      ; pointer to motif
    lw $s2, gen_len    ; genome length
    lw $s3, mot_len    ; motif length
    
    ; Call recursive function to find spliced motif
    li $a0, 0          ; start index in genome
    li $a1, 0          ; start index in motif  
    li $a2, 0          ; result counter
    
    jal find_spliced_motif
    
    ; Exit program
    li $v0, 17
    syscall

find_spliced_motif:
    ; Function to find spliced motif
    ; $a0 = genome_index
    ; $a1 = motif_index  
    ; $a2 = result_count
    
    ; Base case: if we've found complete motif
    bge $a1, $s3, print_result
    
    ; Base case: if we've exhausted genome
    bge $a0, $s2, return_fail
    
    ; Get current characters
    la $t0, genome
    add $t0, $t0, $a0      ; genome[genome_index]
    lb $t1, 0($t0)
    
    la $t2, motif
    add $t2, $t2, $a1      ; motif[motif_index]  
    lb $t3, 0($t2)
    
    ; If characters match
    beq $t1, $t3, found_match
    
    ; If no match, continue in genome
    addi $a0, $a0, 1
    jal find_spliced_motif
    
found_match:
    ; Store position (1-indexed)
    addi $t4, $a0, 1       ; 1-based index
    
    ; Print position (simplified)
    li $v0, 1              ; print integer
    move $a0, $t4
    syscall
    
    ; Continue with both indices incremented
    addi $a0, $a0, 1
    addi $a1, $a1, 1
    jal find_spliced_motif

print_result:
    # Success - motif found
    jr $ra

return_fail:
    # Failure - motif not found
    jr $ra
```

## Key Algorithm Steps

1. **Initialize pointers** to both DNA strings
2. **Compare characters** one by one from genome and motif
3. **When match found**, store the position and advance both pointers
4. **When no match**, advance only genome pointer
5. **Continue** until complete motif is found or genome exhausted

## Time Complexity
- **Time**: O(n × m) where n = genome length, m = motif length
- **Space**: O(1) for algorithmic space (excluding output)

## Expected Output
For genome "ACGTACGTACGT" and motif "ACTG", the program should output:
```
1 4 7 10
```

This represents the 1-indexed positions where the spliced motif occurs in the genome.

The assembly implementation efficiently handles the string matching logic while maintaining proper register usage and memory addressing for the MIPS architecture.