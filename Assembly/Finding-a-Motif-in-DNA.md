# Finding a Motif in DNA - Rosalind Problem Solution

## Problem Understanding

The task is to find all locations (1-indexed) where a given DNA motif appears as a substring in a DNA sequence.

## Assembly Solution

```assembly
.data
    dna_sequence: .ascii "GATATATGCATATACTT"
    dna_length: .long 25
    motif: .ascii "ATAT"
    motif_length: .long 4
    positions: .space 100
    position_count: .long 0
    
.text
.globl _start

_start:
    # Load addresses
    la $s0, dna_sequence      # $s0 = dna sequence address
    lw $s1, dna_length        # $s1 = dna length
    la $s2, motif             # $s2 = motif address
    lw $s3, motif_length      # $s3 = motif length
    
    # Initialize loop counters
    li $t0, 0                 # i = 0 (current position in DNA)
    li $t1, 0                 # position_count = 0
    la $t2, positions         # $t2 = positions array address
    
check_loop:
    # Check if we've reached the end
    add $t3, $t0, $s3         # $t3 = i + motif_length
    bge $t3, $s1, done        # if (i + motif_length > dna_length) then done
    
    # Compare motif with DNA substring
    li $t4, 0                 # j = 0 (character comparison index)
    li $t5, 1                 # match flag = 1 (assume match)
    
compare_loop:
    beq $t4, $s3, match_found # if j == motif_length then match
    
    # Get character from DNA
    add $t6, $s0, $t0         # dna[i]
    add $t7, $t6, $t4         # dna[i + j]
    lb $t8, 0($t7)            # dna_char = dna[i + j]
    
    # Get character from motif
    add $t9, $s2, $t4         # motif[j]
    lb $t10, 0($t9)           # motif_char = motif[j]
    
    # Compare characters
    bne $t8, $t10, no_match   # if dna_char != motif_char then no match
    
    # Continue comparing
    addi $t4, $t4, 1          # j++
    j compare_loop
    
no_match:
    li $t5, 0                 # set match flag to 0
    j next_position
    
match_found:
    # Store position (1-indexed)
    addi $t0, $t0, 1          # Convert to 1-indexed
    sw $t0, 0($t2)            # positions[position_count] = i + 1
    addi $t2, $t2, 4          # move to next position in array
    addi $t1, $t1, 1          # increment count
    
next_position:
    addi $t0, $t0, 1          # i++
    j check_loop
    
done:
    # Print results (simplified version)
    li $v0, 1                 # system call for print integer
    lw $a0, position_count    # load count
    syscall
    
    # Exit program
    li $v0, 10                # system call for exit
    syscall
```

## Alternative Simpler Approach

```assembly
.data
    sequence: .ascii "GATATATGCATATACTT"
    motif: .ascii "ATAT"
    seq_len: .long 25
    motif_len: .long 4
    
.text
.globl _start

_start:
    # Load parameters
    la $s0, sequence
    lw $s1, seq_len
    la $s2, motif
    lw $s3, motif_len
    
    # Loop through each possible start position
    li $t0, 0                 # i = 0 (start position)
    
outer_loop:
    # Check bounds: i + motif_length <= sequence_length
    add $t1, $t0, $s3         # i + motif_len
    bge $t1, $s1, exit        # if i + motif_len >= seq_len, exit
    
    # Compare substring
    li $t2, 0                 # j = 0 (character index)
    li $t3, 1                 # match = 1 (assume match)
    
inner_loop:
    beq $t2, $s3, print_pos   # if j == motif_len, we found a match
    
    # Get sequence character
    add $t4, $s0, $t0         # address of sequence[i]
    add $t5, $t4, $t2         # address of sequence[i + j]
    lb $t6, 0($t5)            # sequence_char
    
    # Get motif character
    add $t7, $s2, $t2         # address of motif[j]
    lb $t8, 0($t7)            # motif_char
    
    # Compare characters
    bne $t6, $t8, next_i      # if not equal, try next i
    
    addi $t2, $t2, 1          # j++
    j inner_loop
    
print_pos:
    # Print position (1-indexed)
    addi $t0, $t0, 1          # Convert to 1-indexed
    li $v0, 1                 # print integer syscall
    move $a0, $t0             # load position
    syscall
    
next_i:
    addi $t0, $t0, 1          # i++
    j outer_loop
    
exit:
    li $v0, 10                # exit syscall
    syscall
```

## Key Concepts Used

1. **String Comparison**: Compare character-by-character between DNA sequence and motif
2. **Loop Control**: Use nested loops to check all possible positions
3. **Memory Access**: Load characters from memory using load byte instructions
4. **Indexing**: Convert 0-indexed positions to 1-indexed as required by problem
5. **Bounds Checking**: Ensure we don't go beyond the sequence length

## Time Complexity
O(n × m) where n is the length of DNA sequence and m is the length of motif

## Space Complexity  
O(1) for computation, O(k) for storing k positions found