# Rosalind Problem: Assessing Assembly Quality with N50 and N75

## Problem Understanding

The N50 and N75 statistics are commonly used to assess assembly quality:
- **N50**: The length of the shortest contig in the set of contigs that contain at least 50% of the total assembly length
- **N75**: The length of the shortest contig in the set of contigs that contain at least 75% of the total assembly length

## Solution Approach

1. Read all contig lengths from input
2. Calculate total assembly length
3. Sort contigs in descending order
4. Find N50 and N75 values by accumulating lengths until thresholds are met

## Assembly Code Solution

```assembly
; Rosalind - Assessing Assembly Quality with N50 and N75
; Input: List of contig lengths (one per line)
; Output: N50 and N75 values

    ; Initialize registers
    mov r1, #0          ; total_length
    mov r2, #0          ; counter
    mov r3, #0          ; temp_length
    
    ; Read all contig lengths into array
    ; (Assuming we have a way to read input)
    
    ; Calculate total length
    mov r4, #0          ; index
    mov r5, #0          ; sum
    
read_loop:
    cmp r4, r2          ; compare index with count
    bge total_done      ; if index >= count, done
    
    ; Load contig length at index r4 into r3
    ; (Implementation depends on memory layout)
    
    add r5, r5, r3      ; sum += contig_length
    add r4, r4, #1      ; increment index
    b read_loop
    
total_done:
    mov r1, r5          ; total_length = sum
    
    ; Sort contigs in descending order
    ; (Sorting algorithm implementation)
    
    ; Calculate N50 and N75
    mov r4, #0          ; accumulated_length
    mov r6, #0          ; index for N50
    mov r7, #0          ; index for N75
    
    ; Find N50 (50% threshold)
    mov r8, r1          ; copy total_length
    lsr r8, r8, #1      ; divide by 2 (50%)
    
    mov r9, #0          ; current_contig_length
    mov r10, #0         ; n50_value
    
n50_loop:
    ; Get contig length from sorted array at index r6
    ; (Assuming we have a function to get sorted contig length)
    
    add r4, r4, r9      ; accumulate_length += current_contig
    cmp r4, r8          ; compare with 50% threshold
    
    bge n50_found       ; if accumulated >= threshold
    add r6, r6, #1      ; increment index
    b n50_loop
    
n50_found:
    mov r10, r9         ; N50 = current contig length
    
    ; Find N75 (75% threshold)
    mov r4, #0          ; reset accumulated_length
    mov r6, #0          ; reset index
    mov r8, r1          ; copy total_length
    mov r11, #2         ; 75% = total/4 * 3
    mul r8, r11         ; multiply by 3
    lsr r8, r8, #2      ; divide by 4 (75%)
    
n75_loop:
    ; Get contig length from sorted array at index r6
    ; (Assuming we have a function to get sorted contig length)
    
    add r4, r4, r9      ; accumulate_length += current_contig
    cmp r4, r8          ; compare with 75% threshold
    
    bge n75_found       ; if accumulated >= threshold
    add r6, r6, #1      ; increment index
    b n75_loop
    
n75_found:
    mov r12, r9         ; N75 = current contig length
    
    ; Output results (N50 and N75)
    ; (Implementation depends on output mechanism)
    
    ; End program
    mov r0, #0          ; return 0
    bx lr               ; return to caller
```

## Alternative Implementation (More Concise)

```assembly
; Simplified approach for N50/N75 calculation

    ; Input: contig lengths in memory array
    ; Output: N50 and N75 values
    
    ; Step 1: Calculate total length
    mov r1, #0          ; total_length = 0
    mov r2, #0          ; index = 0
    
total_loop:
    cmp r2, contig_count
    bge total_done
    
    ldr r3, [contig_array, r2, lsl #2]  ; load contig length (assuming 4-byte integers)
    add r1, r1, r3
    add r2, r2, #1
    b total_loop
    
total_done:
    mov r4, r1          ; r4 = total_length
    
    ; Step 2: Sort contigs in descending order
    ; (Sorting implementation would go here)
    
    ; Step 3: Find N50 and N75
    mov r5, #0          ; accumulated_length = 0
    mov r6, #0          ; index = 0
    
    ; Calculate 50% threshold
    lsr r7, r4, #1      ; r7 = total_length / 2
    
n50_calc:
    ldr r8, [sorted_array, r6, lsl #2]
    add r5, r5, r8
    cmp r5, r7
    bge n50_found
    
    add r6, r6, #1
    b n50_calc
    
n50_found:
    ; N50 = r8 (current contig length)
    
    ; Calculate 75% threshold  
    mov r9, r4          ; copy total_length
    mov r10, #3         ; multiply by 3
    mul r9, r10
    lsr r9, r9, #2      ; divide by 4 (75%)
    
    mov r5, #0          ; reset accumulated_length
    mov r6, #0          ; reset index
    
n75_calc:
    ldr r8, [sorted_array, r6, lsl #2]
    add r5, r5, r8
    cmp r5, r9
    bge n75_found
    
    add r6, r6, #1
    b n75_calc
    
n75_found:
    ; N75 = r8 (current contig length)
```

## Key Points

1. **Input Processing**: Read all contig lengths into memory
2. **Sorting**: Sort contigs in descending order of length
3. **Accumulation**: Keep a running sum while iterating through sorted contigs
4. **Threshold Checking**: Stop when accumulated length reaches 50% or 75% of total
5. **Output**: Return the length of the last contig added to reach threshold

The actual implementation details depend on the specific Assembly dialect and available libraries for input/output operations and sorting algorithms.