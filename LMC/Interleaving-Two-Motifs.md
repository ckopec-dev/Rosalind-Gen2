# Rosalind Problem: Interleaving Two Motifs (LMC Solution)

## Problem Understanding

The problem asks us to find the number of ways to interleave two strings (motifs) such that the resulting string contains both original strings as subsequences.

## LMC Implementation

```lmc
; Interleaving Two Motifs - Rosalind Problem Solution
; LMC Assembly Code

; Data section
    ORG 100
    DAT 0
    DAT 0
    DAT 0
    DAT 0
    DAT 0
    DAT 0
    DAT 0
    DAT 0
    DAT 0
    DAT 0

; Main program
    LDA STRING1_LENGTH
    STA M
    LDA STRING2_LENGTH
    STA N
    
    ; Initialize DP table
    LDA M
    ADD N
    ADD 1
    STA TABLE_SIZE
    
    ; Clear DP table
    LDA 0
    STA DP_TABLE
    LDA 0
    STA DP_TABLE+1
    LDA 0
    STA DP_TABLE+2
    
    ; Fill DP table
    LDA 1
    STA I
    LDA 1
    STA J
    
    ; Outer loop for i
    LOOP_I LDA I
           LDA M
           SUB 1
           BRZ END_I_LOOP
           LDA I
           ADD 1
           STA I
           LDA 1
           STA J
           
           ; Inner loop for j
           LOOP_J LDA J
                  LDA N
                  SUB 1
                  BRZ END_J_LOOP
                  LDA J
                  ADD 1
                  STA J
                  
                  ; Calculate table index
                  LDA I
                  SUB 1
                  MUL N
                  ADD J
                  ADD 1
                  STA INDEX
                  
                  ; Get characters
                  LDA I
                  SUB 1
                  LDA STRING1
                  ADD 1
                  LDA STRING1_CHAR
                  STA CHAR1
                  
                  LDA J
                  SUB 1
                  LDA STRING2
                  ADD 1
                  LDA STRING2_CHAR
                  STA CHAR2
                  
                  ; DP transition
                  LDA CHAR1
                  SUB CHAR2
                  BRZ MATCH_CASE
                  
                  ; No match case
                  LDA DP_TABLE
                  LDA DP_TABLE+1
                  ADD DP_TABLE+2
                  STA DP_TABLE+3
                  BR LOOP_J
                  
                  ; Match case
                  MATCH_CASE LDA DP_TABLE
                             LDA DP_TABLE+1
                             ADD DP_TABLE+2
                             ADD DP_TABLE+3
                             STA DP_TABLE+4
                             BR LOOP_J
                  
                  ; End of inner loop
                  END_J_LOOP LDA J
                             LDA N
                             SUB 1
                             BRZ END_I_LOOP
                             BR LOOP_J
           
           ; End of outer loop
           END_I_LOOP LDA I
                      LDA M
                      SUB 1
                      BRZ DONE
                      BR LOOP_I

DONE    LDA DP_TABLE+4
        HLT

; Data definitions
STRING1     DAT 0
STRING1_LENGTH DAT 0
STRING2     DAT 0
STRING2_LENGTH DAT 0
DP_TABLE    DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0
            DAT 0

; Constants
M           DAT 0
N           DAT 0
I           DAT 0
J           DAT 0
INDEX       DAT 0
CHAR1       DAT 0
CHAR2       DAT 0

; Helper functions
    ; Calculate factorial
FACTORIAL LDA 1
          STA RESULT
          LDA N
          BRZ RETURN_RESULT
          LDA N
          SUB 1
          STA N
          LDA RESULT
          MUL N
          STA RESULT
          BR FACTORIAL
          
RETURN_RESULT LDA RESULT
              HLT

    ; Modular arithmetic
MODULAR LDA A
        MOD B
        STA RESULT
        HLT

    ; Greatest Common Divisor
GCD     LDA A
        LDA B
        SUB 1
        BRZ RETURN_GCD
        LDA B
        LDA A
        MOD B
        STA A
        LDA B
        STA B
        BR GCD
        
RETURN_GCD LDA B
           HLT
```

## Alternative Implementation (More Direct)

```lmc
; Simpler approach for interleaving motifs
; Uses dynamic programming to count valid interleavings

    ORG 100

; Initialize data
    LDA 0
    STA DP_TABLE
    LDA 0
    STA DP_TABLE+1
    LDA 0
    STA DP_TABLE+2
    LDA 0
    STA DP_TABLE+3
    LDA 0
    STA DP_TABLE+4

; Main DP computation
    LDA STRING1_LENGTH
    STA M
    LDA STRING2_LENGTH
    STA N

    ; Initialize first row and column
    LDA 1
    STA I
    LDA 1
    STA J

    ; First row (j=0)
    LDA 1
    STA J
    LDA 1
    STA DP_TABLE
    LDA 1
    STA DP_TABLE+1

    ; First column (i=0)
    LDA 1
    STA I
    LDA 1
    STA DP_TABLE
    LDA 1
    STA DP_TABLE+1

    ; Fill the DP table
    LDA 1
    STA I
    LDA 1
    STA J

    ; Outer loop
    OUTER_LOOP LDA I
               LDA M
               SUB 1
               BRZ END_OUTER
               LDA I
               ADD 1
               STA I
               LDA 1
               STA J

    ; Inner loop
    INNER_LOOP LDA J
               LDA N
               SUB 1
               BRZ END_INNER
               LDA J
               ADD 1
               STA J

               ; Compute DP[i][j]
               LDA I
               SUB 1
               LDA J
               ADD 1
               MUL N
               ADD J
               ADD 1
               STA INDEX

               ; Get characters
               LDA I
               SUB 1
               LDA STRING1
               ADD 1
               LDA STRING1_CHAR
               STA CHAR1

               LDA J
               SUB 1
               LDA STRING2
               ADD 1
               LDA STRING2_CHAR
               STA CHAR2

               ; Update DP table
               LDA CHAR1
               SUB CHAR2
               BRZ MATCH

               ; No match - sum of paths
               LDA DP_TABLE+INDEX-1
               LDA DP_TABLE+INDEX-N
               ADD DP_TABLE+INDEX-N-1
               STA DP_TABLE+INDEX

               BR INNER_LOOP

    MATCH   LDA DP_TABLE+INDEX-1
            LDA DP_TABLE+INDEX-N
            ADD DP_TABLE+INDEX-N-1
            ADD 1
            STA DP_TABLE+INDEX

            BR INNER_LOOP

    END_INNER LDA J
              LDA N
              SUB 1
              BRZ END_OUTER
              BR INNER_LOOP

    END_OUTER LDA I
              LDA M
              SUB 1
              BRZ DONE
              BR OUTER_LOOP

DONE    LDA DP_TABLE+M*N
        HLT

; Data section
STRING1         DAT 0
STRING1_LENGTH  DAT 0
STRING2         DAT 0
STRING2_LENGTH  DAT 0
DP_TABLE        DAT 0
                DAT 0
                DAT 0
                DAT 0
                DAT 0
                DAT 0
                DAT 0
                DAT 0
                DAT 0
                DAT 0
```

## Key Algorithm Steps

1. **Dynamic Programming Approach**: Use a 2D table where `dp[i][j]` represents the number of ways to interleave the first `i` characters of string1 with the first `j` characters of string2.

2. **Base Cases**: 
   - `dp[0][j] = 1` (only one way to interleave empty string with j characters)
   - `dp[i][0] = 1` (only one way to interleave i characters with empty string)

3. **Recurrence Relation**:
   - If `s1[i-1] == s2[j-1]`: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`
   - Otherwise: `dp[i][j] = dp[i-1][j] + dp[i][j-1]`

4. **Final Answer**: `dp[m][n]` where `m` and `n` are lengths of the two strings.

## Time and Space Complexity

- **Time Complexity**: O(m × n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m × n) for the DP table

This solution efficiently computes the number of valid interleavings between two motifs using dynamic programming principles.

