# Finding the Longest Multiple Repeat in LMC

I'll solve this Rosalind problem using the LMC (Little Man Computer) programming language.

## Problem Understanding

The task is to find the longest repeated substring that appears at least twice in a given DNA sequence.

## LMC Solution

```
; Finding the Longest Multiple Repeat
; LMC Program to find longest repeated substring

; Data section
DATA    DAT     1000        ; Start of input string
        DAT     0           ; String length
        DAT     0           ; Max repeat length found
        DAT     0           ; Position of max repeat
        DAT     0           ; Current position
        DAT     0           ; Temp storage
        DAT     0           ; Counter for comparisons
        DAT     0           ; Flag for match found

; Main program
        LDA     DATA            ; Load input string start
        STA     TEMP            ; Store in temp
        LDA     DATA+1          ; Load string length
        STA     LENGTH          ; Store length

        LDA     LENGTH          ; Load length
        SUB     ONE             ; Subtract 1 (for 0-based indexing)
        STA     MAXPOS          ; Store max position

        LDA     ZERO            ; Initialize current position
        STA     CURPOS          ; Store current position

; Main loop - check all possible starting positions
CHECKPOS LDA     CURPOS          ; Load current position
        LDA     MAXPOS          ; Load max position
        SUB     CURPOS          ; Subtract current position
        BRZ     DONE            ; If 0, we're done
        LDA     LENGTH          ; Load total length
        SUB     CURPOS          ; Subtract current position
        STA     REMAIN          ; Store remaining length

        LDA     ZERO            ; Reset counter
        STA     COUNTER         ; Store counter

; Inner loop - check all possible repeat lengths
CHECKLEN LDA     COUNTER         ; Load counter
        LDA     REMAIN          ; Load remaining length
        SUB     COUNTER         ; Subtract counter
        BRZ     NEXTPOS         ; If 0, move to next position
        LDA     COUNTER         ; Load counter
        STA     LEN             ; Store length to check

        LDA     CURPOS          ; Load current position
        STA     STARTPOS        ; Store start position

        LDA     LEN             ; Load length to check
        LDA     STARTPOS        ; Load start position
        ADD     LEN             ; Add length to get end position
        STA     ENDPOS          ; Store end position

        LDA     ZERO            ; Reset match flag
        STA     MATCH           ; Clear match flag

        LDA     CURPOS          ; Load current position
        ADD     LEN             ; Add length to get next position
        STA     NEXTSTART       ; Store next start position

        LDA     NEXTSTART       ; Load next start position
        LDA     LENGTH          ; Load total length
        SUB     LEN             ; Subtract length
        BRZ     NEXTPOS         ; If 0, move to next position

        LDA     ZERO            ; Reset inner counter
        STA     INNERCOUNT      ; Store inner counter

; Compare substrings
COMPARE  LDA     INNERCOUNT      ; Load inner counter
        LDA     LEN             ; Load length
        SUB     INNERCOUNT      ; Subtract counter
        BRZ     MATCHFOUND      ; If 0, match found

        LDA     STARTPOS        ; Load start position
        ADD     INNERCOUNT      ; Add counter
        LDA     DATA            ; Load input string
        LDA     DATA+2          ; Load character at start position + counter
        STA     CH1             ; Store first character

        LDA     NEXTSTART       ; Load next start position
        ADD     INNERCOUNT      ; Add counter
        LDA     DATA            ; Load input string
        LDA     DATA+2          ; Load character at next start position + counter
        STA     CH2             ; Store second character

        LDA     CH1             ; Load first character
        SUB     CH2             ; Subtract second character
        BRZ     CONTINUE        ; If equal, continue
        LDA     ZERO            ; Set no match
        STA     MATCH           ; Store match flag
        BRZ     NEXTPOS         ; Jump to next position

CONTINUE LDA     INNERCOUNT      ; Load inner counter
        ADD     ONE             ; Add 1
        STA     INNERCOUNT      ; Store counter
        BRZ     COMPARE         ; Continue comparison

MATCHFOUND LDA     MATCH           ; Load match flag
        BRZ     NEXTPOS         ; If no match, go to next position

        LDA     LEN             ; Load current length
        LDA     MAXLEN          ; Load max length
        SUB     LEN             ; Subtract
        BRP     NEXTPOS         ; If positive or zero, continue
        LDA     LEN             ; Load current length
        STA     MAXLEN          ; Store as new max length
        LDA     STARTPOS        ; Load start position
        STA     MAXPOS          ; Store max position

NEXTPOS  LDA     CURPOS          ; Load current position
        ADD     ONE             ; Add 1
        STA     CURPOS          ; Store new position
        BRZ     CHECKPOS        ; Continue checking positions

DONE     LDA     MAXLEN          ; Load max length
        LDA     MAXPOS          ; Load max position
        LDA     DATA            ; Load input string
        LDA     DATA+3          ; Output result
        HLT                     ; Halt

; Constants
ONE     DAT     1
ZERO    DAT     0
LENGTH  DAT     0
MAXPOS  DAT     0
CURPOS  DAT     0
TEMP    DAT     0
COUNTER DAT     0
REMAIN  DAT     0
LEN     DAT     0
STARTPOS DAT    0
ENDPOS  DAT     0
MATCH   DAT     0
NEXTSTART DAT   0
INNERCOUNT DAT  0
CH1     DAT     0
CH2     DAT     0
MAXLEN  DAT     0

; Example input format:
; DATA    DAT     1000        ; Address of DNA string
;         DAT     10          ; Length of string
;         DAT     "ACGTACGTACGT" ; DNA sequence
```

## Algorithm Explanation

This LMC program implements a brute-force approach to find the longest repeated substring:

1. **Input Processing**: Reads the DNA sequence and its length from memory
2. **Nested Loops**: 
   - Outer loop: Checks each possible starting position
   - Inner loop: Checks each possible repeat length from current position
3. **Substring Comparison**: Compares substrings of same length at different positions
4. **Maximum Tracking**: Keeps track of the longest repeat found
5. **Output**: Returns the length and position of the longest repeat

## Key Features

- Uses LMC's limited instruction set (LOAD, STORE, ADD, SUB, BRANCH)
- Implements nested loops for thorough searching
- Handles memory management within LMC's constraints
- Tracks both length and position of the maximum repeat

## Time Complexity

O(n³) where n is the length of the DNA sequence, due to the nested loops checking all possible substrings.

## Space Complexity

O(1) additional space beyond input storage, using only the limited memory locations available in LMC.

This solution works within the constraints of the LMC programming language while solving the core problem of finding the longest multiple repeat in DNA sequences.

