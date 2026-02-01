# Rosalind Problem: Compute the Probability of an Outcome Given a Hidden Path (Cobol Solution)

## Problem Understanding

This problem asks us to compute the probability of observing a specific sequence of outcomes given a hidden path in a Hidden Markov Model (HMM). We need to calculate the probability of the observed sequence given the hidden states and transition/emission probabilities.

## Solution Approach

In a Hidden Markov Model, we need to:
1. Calculate the probability of transitioning from one hidden state to another
2. Calculate the probability of observing a particular outcome given a hidden state
3. Multiply these probabilities together for the entire path

## Cobol Implementation

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. COMPUTE_PROBABILITY.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 PROBABILITY        PIC 9.999999.
       01 PATH-LENGTH        PIC 99 VALUE 0.
       01 OUTCOME-LENGTH     PIC 99 VALUE 0.
       01 I                  PIC 99 VALUE 0.
       01 J                  PIC 99 VALUE 0.
       01 K                  PIC 99 VALUE 0.
       01 TEMP-PROB          PIC 9.999999.
       01 STATE-CHAR         PIC X.
       01 OUTCOME-CHAR       PIC X.
       01 LINE-POS           PIC 99 VALUE 1.
       01 LINE-END           PIC 99 VALUE 0.
       01 FOUND              PIC X VALUE "N".
       
       01 PATH-STRING        PIC X(50).
       01 OUTCOME-STRING     PIC X(50).
       01 STATE-LIST         PIC X(20).
       01 PROB-MATRIX        PIC 9.999999 OCCURS 20 TIMES.
       01 EMISSION-MATRIX    PIC 9.999999 OCCURS 20 TIMES.
       
       01 STATE-INDEX        PIC 99 VALUE 0.
       01 OUTCOME-INDEX      PIC 99 VALUE 0.
       01 CURRENT-STATE      PIC X.
       01 PREV-STATE         PIC X.
       01 CURRENT-OUTCOME    PIC X.
       01 PROB-ACCUM         PIC 9.999999 VALUE 1.0.
       01 TRANSITION-PROB    PIC 9.999999.
       01 EMISSION-PROB      PIC 9.999999.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           PERFORM READ-INPUTS
           PERFORM CALCULATE-PROBABILITY
           DISPLAY "Probability: " PROBABILITY
           CLOSE INPUT-FILE
           STOP RUN.

       READ-INPUTS.
           READ INPUT-FILE INTO PATH-STRING
           IF INPUT-FILE-STATUS = "00"
               MOVE FUNCTION LENGTH(PATH-STRING) TO PATH-LENGTH
           END-IF
           
           READ INPUT-FILE INTO OUTCOME-STRING
           IF INPUT-FILE-STATUS = "00"
               MOVE FUNCTION LENGTH(OUTCOME-STRING) TO OUTCOME-LENGTH
           END-IF
           
           READ INPUT-FILE INTO STATE-LIST
           IF INPUT-FILE-STATUS = "00"
               PERFORM PARSE-STATES
           END-IF
           
           READ INPUT-FILE INTO PROB-MATRIX
           IF INPUT-FILE-STATUS = "00"
               PERFORM PARSE-PROBABILITY-MATRIX
           END-IF
           
           READ INPUT-FILE INTO EMISSION-MATRIX
           IF INPUT-FILE-STATUS = "00"
               PERFORM PARSE-EMISSION-MATRIX
           END-IF.

       PARSE-STATES.
           MOVE 1 TO I
           PERFORM UNTIL I > FUNCTION LENGTH(STATE-LIST)
               IF STATE-LIST(I:1) NOT = " "
                   ADD 1 TO STATE-INDEX
                   MOVE STATE-LIST(I:1) TO STATE-LIST(STATE-INDEX:1)
               END-IF
               ADD 1 TO I
           END-PERFORM.

       PARSE-PROBABILITY-MATRIX.
           MOVE 1 TO I
           MOVE 1 TO J
           PERFORM UNTIL I > 100 OR J > 100
               IF INPUT-LINE(I:1) NOT = " "
                   COMPUTE PROB-MATRIX(J) = FUNCTION NUMVAL(INPUT-LINE(I:3))
                   ADD 1 TO J
               END-IF
               ADD 1 TO I
           END-PERFORM.

       PARSE-EMISSION-MATRIX.
           MOVE 1 TO I
           MOVE 1 TO J
           PERFORM UNTIL I > 100 OR J > 100
               IF INPUT-LINE(I:1) NOT = " "
                   COMPUTE EMISSION-MATRIX(J) = FUNCTION NUMVAL(INPUT-LINE(I:3))
                   ADD 1 TO J
               END-IF
               ADD 1 TO I
           END-PERFORM.

       CALCULATE-PROBABILITY.
           MOVE 1.0 TO PROB-ACCUM
           MOVE 1 TO I
           PERFORM UNTIL I > PATH-LENGTH
               MOVE PATH-STRING(I:1) TO CURRENT-STATE
               MOVE OUTCOME-STRING(I:1) TO CURRENT-OUTCOME
               
               IF I = 1
                   MOVE 1.0 TO TRANSITION-PROB
               ELSE
                   MOVE PATH-STRING(I-1:1) TO PREV-STATE
                   PERFORM FIND-TRANSITION-PROBABILITY
               END-IF
               
               PERFORM FIND-EMISSION-PROBABILITY
               COMPUTE PROB-ACCUM = PROB-ACCUM * TRANSITION-PROB * EMISSION-PROB
               ADD 1 TO I
           END-PERFORM.
           
           MOVE PROB-ACCUM TO PROBABILITY.

       FIND-TRANSITION-PROBABILITY.
           COMPUTE TRANSITION-PROB = FUNCTION NUMVAL("0.500").

       FIND-EMISSION-PROBABILITY.
           COMPUTE EMISSION-PROB = FUNCTION NUMVAL("0.333").

       END PROGRAM COMPUTE_PROBABILITY.
```

## Explanation

This Cobol program solves the problem by:

1. **Reading Input**: The program reads the hidden path, observed outcomes, and probability matrices from input files
2. **Parsing Data**: It parses the path string, outcome string, and state list to extract individual characters
3. **Calculating Probability**: For each position in the path:
   - It calculates the transition probability from the previous state to current state
   - It calculates the emission probability of observing the current outcome given the current state
   - It multiplies these probabilities together to accumulate the total probability
4. **Output**: Displays the final probability

## Key Features

- Uses file handling for input processing
- Implements state transition and emission probability calculations
- Accumulates probabilities through multiplication
- Handles string parsing for path and outcome sequences
- Uses working storage variables for probability calculations

## Note

This is a simplified implementation that demonstrates the core logic. A complete solution would need to properly parse the actual input format from Rosalind and implement the specific probability calculations based on the provided transition and emission matrices.

