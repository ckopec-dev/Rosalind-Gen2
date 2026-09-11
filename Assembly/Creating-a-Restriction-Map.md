# Rosalind Problem: Creating a Restriction Map

## Problem Description
Given a set of restriction enzyme cut sites and their positions, we need to create a restriction map showing the distances between consecutive cuts.

## Approach
1. Parse input data to extract cut site positions
2. Sort the positions in ascending order
3. Calculate distances between consecutive cuts
4. Output the restriction map

## Solution

```assembly
; Creating a Restriction Map - Assembly Solution
; Input: List of cut positions
; Output: Distances between consecutive cuts

.data
    ; Sample input data (positions of restriction enzyme cuts)
    cuts: .word 0, 15, 35, 50, 75, 100
    num_cuts: .word 6
    
    ; Buffer for output
    buffer: .space 256
    
    ; String literals
    newline: .ascii "\n"
    space: .ascii " "
    
.text
.globl _start

_start:
    ; Load base addresses
    la $t0, cuts          ; $t0 = address of cuts array
    lw $t1, num_cuts      ; $t1 = number of cuts
    
    ; Initialize loop counter
    li $t2, 0             ; i = 0
    
    ; Check if we have at least 2 cuts to calculate distances
    li $t3, 2
    blt $t1, $t3, done    ; If less than 2 cuts, exit
    
    ; First cut is always at position 0 (start of sequence)
    lw $t4, 0($t0)        ; Load first cut position
    
    ; Loop through cuts to calculate distances
    li $t5, 1             ; Start from second cut (index 1)
    
calculate_distances:
    bge $t5, $t1, done    ; If i >= num_cuts, exit loop
    
    ; Calculate distance between current and previous cut
    lw $t6, 0($t0)        ; Load current cut position
    addi $t0, $t0, 4      ; Move to next cut position
    
    lw $t7, 0($t0)        ; Load next cut position
    sub $t8, $t7, $t6     ; Distance = next - current
    
    ; Print the distance
    li $v0, 1             ; System call for print integer
    move $a0, $t8         ; Move distance to argument register
    syscall
    
    ; Print space separator
    li $v0, 11            ; System call for print character
    li $a0, 32            ; Space character
    syscall
    
    addi $t5, $t5, 1      ; Increment counter
    j calculate_distances

done:
    ; Print newline
    li $v0, 11            ; System call for print character
    li $a0, 10            ; Newline character
    syscall
    
    ; Exit program
    li $v0, 1             ; System call for exit
    li $a0, 0
    syscall
```

## Alternative Solution (More Complete)

```assembly
; More robust restriction map solution

.data
    ; Example cut positions - could be read from input
    cut_positions: .word 0, 15, 35, 50, 75, 100
    num_cuts: .word 6
    
    ; Output formatting strings
    output_format: .ascii "Distances: %d %d %d %d %d\n"
    newline: .ascii "\n"

.text
.globl _start

_start:
    la $s0, cut_positions   ; Load base address of cuts
    lw $s1, num_cuts        ; Load number of cuts
    
    ; Validate input
    li $t0, 1
    bge $s1, $t0, process_cuts
    
    ; Not enough cuts to process
    li $v0, 1
    li $a0, 0
    syscall
    j exit_program

process_cuts:
    li $t1, 0               ; i = 0 (current cut index)
    li $t2, 1               ; next_cut_index = 1
    
    ; Check if we have at least 2 cuts
    li $t3, 2
    blt $s1, $t3, print_done
    
    ; Calculate distances between consecutive cuts
    li $t4, 0               ; distance accumulator
    
calculate_loop:
    bge $t2, $s1, print_done
    
    ; Load current cut position
    sll $t5, $t1, 2         ; offset = i * 4 (word size)
    add $t6, $s0, $t5       ; address of current cut
    lw $t7, 0($t6)          ; current_cut = cut_positions[i]
    
    ; Load next cut position
    sll $t8, $t2, 2         ; offset = (i+1) * 4
    add $t9, $s0, $t8       ; address of next cut
    lw $t10, 0($t9)         ; next_cut = cut_positions[i+1]
    
    ; Calculate distance
    sub $t11, $t10, $t7     ; distance = next_cut - current_cut
    
    ; Print the distance (simplified version)
    li $v0, 1               ; print integer syscall
    move $a0, $t11          ; distance to print
    syscall
    
    ; Print space separator
    li $v0, 11              ; print character syscall
    li $a0, 32              ; space character
    syscall
    
    ; Increment counters
    addi $t1, $t1, 1        ; i++
    addi $t2, $t2, 1        ; next_cut_index++
    
    j calculate_loop

print_done:
    ; Print final newline
    li $v0, 11              ; print character syscall
    li $a0, 10              ; newline character
    syscall
    
exit_program:
    li $v0, 1               ; exit syscall
    li $a0, 0
    syscall
```

## Key Concepts

1. **Array Access**: Use base address + offset to access array elements
2. **Pointer Arithmetic**: Calculate memory addresses using word size (4 bytes)
3. **Loop Control**: Use counters and branch instructions for iteration
4. **Distance Calculation**: Simple subtraction between consecutive positions
5. **Output Formatting**: Print integers with appropriate separators

## Sample Input/Output

**Input**: Cut positions [0, 15, 35, 50, 75, 100]

**Output**: Distances: 15 20 15 25 25

This solution demonstrates how to:
- Process arrays in assembly
- Calculate differences between values
- Handle loop structures
- Generate formatted output