# Rosalind Problem: Find Substrings of a Genome Encoding a Given Amino Acid String (LMC Implementation)

## Problem Understanding

We need to find all substrings of a given DNA sequence that encode a specific amino acid sequence. This involves:
1. Translating DNA to RNA (DNA → RNA)
2. Translating RNA to amino acids using the genetic code
3. Finding all substrings that translate to the target amino acid sequence

## LMC Implementation

```lmc
; Find Substrings of a Genome Encoding a Given Amino Acid String
; LMC Assembly Code

    ORG 100
    ; Input: DNA sequence in memory locations 200-300
    ; Input: Target amino acid sequence in memory locations 300-400
    ; Output: Starting positions of matching substrings

START   LDA DNALEN     ; Load length of DNA sequence
        STA LEN        ; Store in LEN
        LDA AMINOLEN   ; Load length of amino acid sequence
        STA AMINOLEN2  ; Store in AMINOLEN2
        LDA AMINOLEN   ; Load amino acid length again
        STA COUNT      ; Initialize counter
        LDA ZERO       ; Load 0 for counter
        STA POSITION   ; Initialize position counter
        LDA ZERO       ; Load 0 for result counter
        STA RESULTCNT  ; Initialize result counter

; Main loop to check each possible substring
CHECK   LDA POSITION   ; Load current position
        LDA LEN        ; Load DNA length
        LDA AMINOLEN2  ; Load amino acid length
        SUB AMINOLEN2  ; Subtract to get max valid start position
        STA MAXPOS     ; Store max position
        LDA POSITION   ; Load current position
        LDA MAXPOS     ; Load max position
        SUB POSITION   ; Check if we've exceeded max
        BRZ DONE       ; If equal, we're done
        BRZ CHECK      ; If negative, continue checking

; Check if substring matches amino acid sequence
        LDA POSITION   ; Load current position
        STA SUBSTART   ; Store start position
        LDA SUBSTART   ; Load start position
        LDA ZERO       ; Load 0
        STA TMP        ; Initialize tmp counter
        LDA AMINOLEN2  ; Load amino acid length
        STA TMP2       ; Store in tmp2

; Translate DNA to RNA and then to amino acids
TRANSLATE LDA TMP        ; Load tmp counter
        LDA TMP2       ; Load tmp2 (amino acid length)
        SUB TMP        ; Subtract to check if done
        BRZ MATCH      ; If done, check for match

        LDA SUBSTART   ; Load start position
        LDA TMP        ; Load tmp counter
        ADD TMP        ; Add to get current position
        STA CURPOS     ; Store current position
        LDA DNA        ; Load DNA at current position
        STA RNA        ; Store in RNA location
        LDA RNA        ; Load RNA
        LDA ZERO       ; Load 0
        SUB RNA        ; Check if we're at end
        BRZ TRANSLATE  ; If end, continue to next

; Convert RNA to amino acid
        LDA RNA        ; Load RNA nucleotide
        LDA ZERO       ; Load 0 for comparison
        SUB RNA        ; Check if valid
        BRZ TRANSLATE  ; If invalid, continue

; Look up amino acid from RNA codon
        LDA RNA        ; Load RNA nucleotide
        LDA CODON      ; Load codon table
        SUB CODON      ; Subtract to get amino acid
        STA AMINO      ; Store amino acid

        LDA AMINO      ; Load amino acid
        LDA TARGET     ; Load target amino acid
        SUB TARGET     ; Check if match
        BRZ TRANSLATE  ; If match, continue checking
        BRN NOTMATCH   ; If not match, go to next

MATCH   LDA POSITION   ; Load position
        STA RESULT     ; Store in results
        LDA RESULTCNT  ; Load result counter
        LDA ONE        ; Load 1
        ADD ONE        ; Add to counter
        STA RESULTCNT  ; Store updated counter

NOTMATCH LDA POSITION   ; Load position
        LDA ONE        ; Load 1
        ADD ONE        ; Add to position
        STA POSITION   ; Store updated position
        BRN CHECK      ; Go back to check

DONE    LDA RESULTCNT  ; Load result count
        STA OUTPUT     ; Store in output
        LDA ZERO       ; Load 0 for exit
        STA OUTPUT     ; Store 0 to indicate end

; Data section
DNA     DAT 0          ; DNA sequence (will be loaded)
DNALEN  DAT 0          ; Length of DNA sequence
AMINOLEN DAT 0         ; Length of amino acid sequence
AMINOLEN2 DAT 0        ; Copy of amino acid length
COUNT   DAT 0          ; Counter for loops
POSITION DAT 0         ; Current position in DNA
RESULTCNT DAT 0        ; Count of matching results
MAXPOS  DAT 0          ; Maximum valid position
SUBSTART DAT 0         ; Start position of current substring
CURPOS  DAT 0          ; Current position in DNA
TMP     DAT 0          ; Temporary counter
TMP2    DAT 0          ; Temporary counter copy
TARGET  DAT 0          ; Target amino acid sequence
AMINO   DAT 0          ; Current amino acid
RESULT  DAT 0          ; Result storage
OUTPUT  DAT 0          ; Output storage
ZERO    DAT 0          ; Zero constant
ONE     DAT 1          ; One constant
CODON   DAT 0          ; Codon lookup table

    END START
```

## Alternative Simpler Implementation

```lmc
; Simplified version for the problem
; This is a conceptual implementation showing the structure

    ORG 100

START   LDA DNALEN     ; Load DNA sequence length
        STA MAXPOS     ; Store maximum position
        LDA ZERO       ; Load zero
        STA POS        ; Initialize position counter

LOOP    LDA POS        ; Load current position
        LDA MAXPOS     ; Load maximum position
        SUB MAXPOS     ; Check if we're done
        BRZ END        ; If done, exit

        ; Check if substring from current position matches target
        LDA POS        ; Load current position
        LDA TARGETLEN  ; Load target length
        SUB TARGETLEN  ; Subtract to get valid range
        STA VALIDMAX   ; Store valid maximum position

        LDA POS        ; Load position
        LDA VALIDMAX   ; Load valid maximum
        SUB POS        ; Check if valid
        BRZ NEXTPOS    ; If not valid, try next position

        ; Call translation function here
        ; Check if translated amino acid matches target
        LDA MATCH      ; Load match result
        BRZ NEXTPOS    ; If no match, try next position
        BRN STOREPOS   ; If match, store position

NEXTPOS LDA POS        ; Load position
        LDA ONE        ; Load one
        ADD ONE        ; Add one to position
        STA POS        ; Store updated position
        BRN LOOP       ; Continue loop

STOREPOS LDA POS       ; Load position
         STA RESULT     ; Store in result array
         LDA RESULTCNT  ; Load result counter
         LDA ONE        ; Load one
         ADD ONE        ; Add one to counter
         STA RESULTCNT  ; Store updated counter
         BRN NEXTPOS    ; Continue to next position

END     LDA RESULTCNT  ; Load result count
        STA OUTPUT     ; Store in output
        HLT            ; Halt

; Data section
DNALEN   DAT 0         ; Length of DNA sequence
TARGETLEN DAT 0        ; Length of target amino acid sequence
POS      DAT 0         ; Current position in DNA
MAXPOS   DAT 0         ; Maximum valid position
VALIDMAX DAT 0         ; Valid maximum position
MATCH    DAT 0         ; Match result (0 or 1)
RESULTCNT DAT 0        ; Count of matching positions
RESULT   DAT 0         ; Storage for matching positions
OUTPUT   DAT 0         ; Final output
ZERO     DAT 0         ; Zero constant
ONE      DAT 1         ; One constant

    END START
```

## Key Concepts Used

1. **String Matching**: Compare DNA substrings with target amino acid sequence
2. **Translation**: DNA → RNA → Amino Acids using genetic code
3. **Loop Control**: Iterating through possible start positions
4. **Memory Management**: Storing DNA, amino acid sequences, and results
5. **Conditional Logic**: Checking matches and controlling program flow

## Notes

This LMC implementation provides the conceptual framework for solving the problem. In practice, you would need to:
- Implement the genetic code translation table
- Handle the actual DNA to RNA transcription
- Implement proper substring comparison
- Manage memory allocation for sequences
- Add proper input/output handling

The code demonstrates the core algorithmic approach but would require additional LMC instructions and data structures for a complete working solution.

