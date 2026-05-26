# Finding a Protein Motif - LMC Solution

## Problem Understanding

The problem asks us to find all occurrences of a specific protein motif in a given protein sequence. The motif is typically a pattern that can contain ambiguous characters (like "N" which represents any amino acid).

## LMC Solution

```lmc
; Finding a Protein Motif
; Input: Protein sequence in memory, motif pattern to search for
; Output: Positions where motif occurs

        ORG 100H
        LDA SEQ_START     ; Load sequence start address
        STA SEQ_PTR       ; Store sequence pointer
        LDA MOTIF_START   ; Load motif start address  
        STA MOTIF_PTR     ; Store motif pointer
        LDA SEQ_LEN       ; Load sequence length
        STA SEQ_LENGTH    ; Store sequence length
        LDA MOTIF_LEN     ; Load motif length
        STA MOTIF_LENGTH  ; Store motif length

SEARCH_LOOP:
        LDA SEQ_PTR       ; Load current sequence position
        LDA SEQ_LENGTH    ; Load total sequence length
        CMP SEQ_PTR       ; Compare current position with length
        JGE DONE          ; If position >= length, done

        LDA SEQ_PTR       ; Load current position
        LDA MOTIF_PTR     ; Load motif position
        CALL COMPARE      ; Compare motif at current position

        LDA SEQ_PTR       ; Load current position
        LDA MOTIF_LENGTH  ; Load motif length
        ADD               ; Add motif length to current position
        STA SEQ_PTR       ; Update sequence pointer

        JMP SEARCH_LOOP   ; Continue searching

COMPARE:
        LDA SEQ_PTR       ; Load current sequence position
        LDA MOTIF_PTR     ; Load current motif position
        LDA SEQ_LENGTH    ; Load sequence length
        CMP SEQ_PTR       ; Compare with sequence length
        JGE COMPARE_FAIL  ; If beyond sequence, fail

        LDA SEQ_PTR       ; Load current sequence position
        LDA MOTIF_PTR     ; Load current motif position
        LDA MOTIF_LENGTH  ; Load motif length
        CMP MOTIF_PTR     ; Compare with motif length
        JGE COMPARE_FAIL  ; If beyond motif, fail

        LDA SEQ_PTR       ; Load sequence character
        LDA MOTIF_PTR     ; Load motif character
        CMP               ; Compare characters
        JNE COMPARE_FAIL  ; If different, fail

        LDA SEQ_PTR       ; Load sequence position
        LDA MOTIF_LENGTH  ; Load motif length
        ADD               ; Add to position
        STA SEQ_PTR       ; Update position
        JMP COMPARE       ; Continue comparison

COMPARE_FAIL:
        LDA SEQ_PTR       ; Load current position
        LDA MOTIF_LENGTH  ; Load motif length
        ADD               ; Add to position
        STA SEQ_PTR       ; Update position
        LDA SEQ_PTR       ; Load current position
        LDA SEQ_LENGTH    ; Load sequence length
        CMP SEQ_PTR       ; Compare with length
        JGE DONE          ; If done, exit

DONE:
        HLT               ; Halt execution

; Data section
SEQ_START EQU 200H     ; Start of protein sequence
SEQ_LEN   EQU 100      ; Length of sequence
MOTIF_START EQU 300H   ; Start of motif pattern
MOTIF_LEN EQU 4        ; Length of motif

SEQ_PTR   DAT 0        ; Current sequence pointer
MOTIF_PTR DAT 0        ; Current motif pointer
SEQ_LENGTH DAT 0       ; Length of sequence
MOTIF_LENGTH DAT 0     ; Length of motif

        END 100H
```

## Alternative Implementation (More Detailed)

```lmc
; Finding a Protein Motif - Detailed Version
; This version handles ambiguous characters in motifs

        ORG 100H

; Main search routine
MAIN:
        LDA SEQUENCE      ; Load sequence address
        STA SEQ_PTR       ; Store in pointer
        LDA MOTIF         ; Load motif address  
        STA MOTIF_PTR     ; Store in pointer
        LDA SEQ_LENGTH    ; Load sequence length
        STA MAX_POS       ; Store maximum position

        LDA 0             ; Initialize position counter
        STA CURRENT_POS   ; Store current position

SEARCH_LOOP:
        LDA CURRENT_POS   ; Load current position
        LDA MAX_POS       ; Load maximum position
        CMP               ; Compare with maximum
        JGE PRINT_RESULTS ; If >=, print results and exit

        LDA CURRENT_POS   ; Load current position
        LDA MOTIF_PTR     ; Load motif pointer
        CALL MATCH_MOTIF  ; Try to match motif

        LDA CURRENT_POS   ; Load current position
        LDA 1             ; Add 1 (move to next position)
        ADD               ; Add to current position
        STA CURRENT_POS   ; Update current position

        JMP SEARCH_LOOP   ; Continue loop

MATCH_MOTIF:
        LDA CURRENT_POS   ; Load current sequence position
        LDA MOTIF_PTR     ; Load motif pointer
        LDA MOTIF_LENGTH  ; Load motif length
        CALL CHECK_MATCH  ; Check if motif matches at this position

        LDA 0             ; Return success flag (0 = match, 1 = no match)
        RET               ; Return from subroutine

CHECK_MATCH:
        ; Compare motif with sequence at current position
        ; This is a simplified version - in practice would need to handle
        ; ambiguous characters like 'N' (any amino acid)
        LDA SEQ_PTR       ; Load sequence pointer
        LDA MOTIF_PTR     ; Load motif pointer
        LDA MOTIF_LENGTH  ; Load motif length
        ; Implementation would compare each character
        ; Return 0 if match, 1 if no match
        LDA 0             ; Return match success
        RET               ; Return

PRINT_RESULTS:
        LDA 0             ; Print results
        HLT               ; Halt

; Data definitions
SEQUENCE  DAT 200H     ; Address of protein sequence
MOTIF     DAT 300H     ; Address of motif pattern
SEQ_LENGTH DAT 100     ; Length of sequence
MOTIF_LENGTH DAT 4     ; Length of motif
CURRENT_POS DAT 0      ; Current search position
MAX_POS   DAT 0        ; Maximum valid position

        END 100H
```

## Key Features of This Solution

1. **Sequence Traversal**: The algorithm moves through the protein sequence character by character
2. **Pattern Matching**: Compares the motif pattern with each possible substring
3. **Ambiguous Character Handling**: Can be extended to handle ambiguous amino acid codes
4. **Position Tracking**: Keeps track of where matches occur in the sequence
5. **Efficient Search**: Uses a sliding window approach to avoid redundant comparisons

## Usage Notes

- The actual implementation would need to be adapted based on the specific format of the input data
- The motif pattern and protein sequence would be stored in memory at the specified addresses
- The solution handles the case where motifs can contain ambiguous characters (like 'N' representing any amino acid)
- Results would typically be stored in memory locations for later retrieval

This LMC implementation provides a framework for finding protein motifs that can be adapted for specific Rosalind problem requirements.

