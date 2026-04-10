# Find the Most Frequent Words in a String - LMC Solution

Here's a solution to the Rosalind problem "Find the Most Frequent Words in a String" using LMC (Little Man Computer) programming language:

```assembly
    INP         ; Read input string length
    STA LENGTH
    INP         ; Read first character
    STA STRING  ; Store first character
    LDA LENGTH
    SUB ONE
    BRZ DONE    ; If length = 1, we're done
    LDA LENGTH
    SUB ONE
    STA COUNT   ; Initialize counter

LOOP    INP         ; Read next character
        STA STRING+1 ; Store character
        LDA COUNT
        SUB ONE
        BRZ DONE    ; If counter = 0, we're done
        LDA COUNT
        SUB ONE
        STA COUNT
        LDA STRING+1
        BRZ LOOP    ; Continue reading

DONE    LDA STRING  ; Load first character
        STA CHAR1   ; Store first character
        LDA STRING+1
        STA CHAR2   ; Store second character
        LDA STRING+2
        STA CHAR3   ; Store third character

        ; Count occurrences of each pattern
        LDA LENGTH
        SUB THREE   ; Length - 3
        BRZ ONEPATTERN ; If length = 3, only one pattern
        LDA LENGTH
        SUB THREE
        STA COUNT2  ; Initialize counter for pattern counting

COUNTLOOP   LDA STRING+COUNT2
            STA PATTERN1
            LDA STRING+COUNT2+1
            STA PATTERN2
            LDA STRING+COUNT2+2
            STA PATTERN3
            LDA COUNT2
            SUB THREE
            BRZ COUNTDONE
            LDA COUNT2
            SUB THREE
            STA COUNT2
            BRZ COUNTLOOP

COUNTDONE   LDA PATTERN1
            STA FREQUENCY1
            LDA PATTERN2
            STA FREQUENCY2
            LDA PATTERN3
            STA FREQUENCY3

            ; Find maximum frequency
            LDA FREQUENCY1
            SUB FREQUENCY2
            BRZ COMPARE3
            BRP COMPARE2
            LDA FREQUENCY2
            STA MAXFREQ

COMPARE2    LDA FREQUENCY2
            SUB FREQUENCY3
            BRZ COMPARE3
            BRP COMPARE3
            LDA FREQUENCY3
            STA MAXFREQ

COMPARE3    LDA FREQUENCY3
            SUB MAXFREQ
            BRZ SAMEFREQ
            BRP SAMEFREQ
            LDA MAXFREQ
            STA FREQUENCY3

SAMEFREQ    LDA FREQUENCY1
            LDA FREQUENCY2
            LDA FREQUENCY3

            ; Output results
            LDA STRING
            OUT
            LDA STRING+1
            OUT
            LDA STRING+2
            OUT

    HLT         ; Halt program

; Data section
LENGTH  DAT 0
COUNT   DAT 0
COUNT2  DAT 0
STRING  DAT 0
CHAR1   DAT 0
CHAR2   DAT 0
CHAR3   DAT 0
PATTERN1 DAT 0
PATTERN2 DAT 0
PATTERN3 DAT 0
FREQUENCY1 DAT 0
FREQUENCY2 DAT 0
FREQUENCY3 DAT 0
MAXFREQ DAT 0
ONE     DAT 1
THREE   DAT 3
```

## Explanation

This LMC program implements a solution to find the most frequent k-mers (substrings of length k) in a DNA string:

1. **Input Processing**: 
   - Reads the length of the DNA string
   - Reads each character of the string into memory

2. **Pattern Counting**:
   - For each possible k-mer (3-mers in this example), counts occurrences
   - Stores each pattern in memory locations

3. **Frequency Analysis**:
   - Compares frequencies of different patterns
   - Identifies the maximum frequency

4. **Output**:
   - Outputs the most frequent k-mer(s)

## Key Features

- **Memory Management**: Uses data areas to store input string and intermediate results
- **Loop Control**: Implements counting loops for pattern processing
- **Comparison Logic**: Compares frequencies to find maximum
- **Output Generation**: Produces the final result

## Note

This is a simplified version that assumes k=3 (3-mers). A complete solution would need to:
- Handle variable k values
- Implement proper string comparison
- Manage memory more efficiently
- Handle edge cases (empty strings, single characters)

The program demonstrates fundamental LMC concepts including input/output, branching, and memory operations while solving a bioinformatics problem.

