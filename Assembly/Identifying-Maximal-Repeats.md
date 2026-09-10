# Rosalind Problem: Identifying Maximal Repeats

## Problem Description
Given a string, find all maximal repeats - substrings that appear at least twice and cannot be extended to the left or right while maintaining the repeat property.

## Solution in Assembly Language

```assembly
; Function to find maximal repeats in a string
; Input: String in memory, length in register
; Output: List of maximal repeats

MAXIMAL_REPEATS:
    ; Initialize registers
    MOV R1, #0          ; i = 0 (start index)
    MOV R2, #0          ; j = 0 (end index)  
    MOV R3, #0          ; repeat count
    MOV R4, #0          ; max_length
    
    ; Loop through all possible substrings
    OUTER_LOOP:
        CMP R1, LENGTH      ; Check if i >= length
        BGE END_PROGRAM     ; If yes, exit
        
        MOV R2, R1          ; j = i
        ADD R2, #1          ; j = i + 1
        
        INNER_LOOP:
            CMP R2, LENGTH      ; Check if j >= length
            BGE NEXT_I          ; If yes, go to next i
            
            ; Extract substring from i to j
            CALL EXTRACT_SUBSTRING
            MOV R5, R0          ; Store substring in R5
            
            ; Count occurrences of this substring
            CALL COUNT_OCCURRENCES
            MOV R3, R0          ; Store count in R3
            
            ; Check if repeat is maximal
            CMP R3, #2          ; At least 2 occurrences?
            BLT NEXT_J          ; If less than 2, continue
            
            ; Check if it's maximal by trying to extend left and right
            CALL IS_MAXIMAL
            CMP R0, #1          ; If maximal
            BEQ ADD_TO_RESULTS  ; Add to results
            
        NEXT_J:
            ADD R2, #1          ; j++
            B INNER_LOOP
        
    NEXT_I:
        ADD R1, #1          ; i++
        B OUTER_LOOP
    
    END_PROGRAM:
        ; Return results or terminate
        RET

; Function to extract substring from start to end
EXTRACT_SUBSTRING:
    ; Input: R1 = start index, R2 = end index
    ; Output: R0 = pointer to substring
    MOV R0, #0          ; Initialize result pointer
    
    ; Calculate length of substring
    SUB R6, R2, R1      ; R6 = end - start
    
    ; Copy characters from original string
    MOV R7, R1          ; Current position in original
    MOV R8, #0          ; Position in new substring
    
    EXTRACT_LOOP:
        CMP R8, R6      ; Check if we've copied all chars
        BEQ EXTRACT_END
        
        ; Copy character from original to new string
        MOV R9, ORIGINAL_STRING[R7]  ; Get char from original
        SUBSTRING[R8] = R9           ; Store in substring
        
        ADD R7, #1      ; Move to next char in original
        ADD R8, #1      ; Move to next position in substring
        B EXTRACT_LOOP
    
    EXTRACT_END:
        MOV R0, #SUBSTRING  ; Return pointer to substring
        RET

; Function to count occurrences of substring in main string
COUNT_OCCURRENCES:
    ; Input: R5 = pointer to substring
    ; Output: R0 = count of occurrences
    MOV R0, #0          ; Initialize count
    
    ; Loop through main string
    MOV R1, #0          ; i = 0
    COUNT_LOOP:
        CMP R1, LENGTH      ; Check if we've reached end
        BGE COUNT_END       ; If yes, exit
        
        ; Compare substring with current position
        CALL COMPARE_SUBSTRINGS
        CMP R0, #1          ; If match found
        BEQ INCREMENT_COUNT ; Increment count
        
    COUNT_NEXT:
        ADD R1, #1          ; i++
        B COUNT_LOOP
    
    INCREMENT_COUNT:
        ADD R0, #1          ; Increment count
        B COUNT_NEXT
    
    COUNT_END:
        RET

; Function to check if repeat is maximal
IS_MAXIMAL:
    ; Input: R5 = pointer to substring
    ; Output: R0 = 1 if maximal, 0 if not
    MOV R0, #1          ; Assume maximal initially
    
    ; Check left extension
    CALL CHECK_LEFT_EXTENSION
    CMP R0, #0          ; If extension possible
    BEQ NOT_MAXIMAL     ; Then not maximal
    
    ; Check right extension  
    CALL CHECK_RIGHT_EXTENSION
    CMP R0, #0          ; If extension possible
    BEQ NOT_MAXIMAL     ; Then not maximal
    
    RET                 ; Return 1 (maximal)
    
    NOT_MAXIMAL:
        MOV R0, #0      ; Not maximal
        RET

; Helper function to check left extension
CHECK_LEFT_EXTENSION:
    ; Check if we can extend substring to the left
    ; Implementation depends on specific requirements
    MOV R0, #1          ; Placeholder - return 1 (can extend)
    RET

; Helper function to check right extension
CHECK_RIGHT_EXTENSION:
    ; Check if we can extend substring to the right  
    ; Implementation depends on specific requirements
    MOV R0, #1          ; Placeholder - return 1 (can extend)
    RET

; Function to add repeat to results list
ADD_TO_RESULTS:
    ; Add current substring to maximal repeats list
    ; Implementation for storing results
    RET

; Main program entry point
MAIN:
    ; Initialize input string
    MOV ORIGINAL_STRING, #INPUT_STRING
    
    ; Call main algorithm
    CALL MAXIMAL_REPEATS
    
    ; Process and output results
    ; (Implementation depends on specific output format)
    
    RET
```

## Key Algorithm Steps:

1. **Generate all substrings**: For each starting position i and ending position j, extract substring from i to j
2. **Count occurrences**: For each substring, count how many times it appears in the main string
3. **Filter repeats**: Keep only substrings that appear at least twice
4. **Check maximality**: Ensure substring cannot be extended left or right while maintaining repeat property
5. **Output results**: Return all maximal repeats found

## Time Complexity:
- O(n³) where n is the length of the string
- O(n²) for generating substrings and counting occurrences
- O(n) for checking maximality

## Space Complexity: 
- O(n²) for storing substrings and counts

This assembly implementation provides a framework for solving the maximal repeats problem, with placeholder functions that would need to be implemented based on specific hardware constraints and memory management requirements.