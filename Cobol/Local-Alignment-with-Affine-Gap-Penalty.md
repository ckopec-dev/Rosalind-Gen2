# Local Alignment with Affine Gap Penalty in COBOL

I'll solve the Rosalind problem Local_Alignment_with_Affine_Gap_Penalty using COBOL. This problem requires finding the optimal local alignment with affine gap penalties.

## Problem Understanding

The task is to find the maximum score for local alignment between two sequences using affine gap penalties (linear gap cost: -11 for opening, -1 for extension).

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCAL_ALIGNMENT_AFFINE.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.
           SELECT OUTPUT-FILE ASSIGN TO "output.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       FD OUTPUT-FILE.
       01 OUTPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 SEQUENCE1 PIC X(50).
       01 SEQUENCE2 PIC X(50).
       01 SEQ1-LENGTH PIC 99 VALUE 0.
       01 SEQ2-LENGTH PIC 99 VALUE 0.

       01 SCORE-MATRIX.
           05 SCORE-CELLS PIC 9(5) OCCURS 51 TIMES DEPENDING ON SEQ2-LENGTH.
           05 SCORE-CELLS-2 PIC 9(5) OCCURS 51 TIMES DEPENDING ON SEQ2-LENGTH.
           05 SCORE-CELLS-3 PIC 9(5) OCCURS 51 TIMES DEPENDING ON SEQ2-LENGTH.

       01 GAP-OPEN PIC 99 VALUE 11.
       01 GAP-EXTEND PIC 99 VALUE 1.
       01 MAX-SCORE PIC 9(5) VALUE 0.
       01 MAX-SCORE-POS PIC 99 VALUE 0.
       01 MAX-SCORE-POS2 PIC 99 VALUE 0.

       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 TEMP-SCORE PIC 9(5) VALUE 0.
       01 MATCH-SCORE PIC 99 VALUE 0.
       01 MISMATCH-SCORE PIC 99 VALUE 0.
       01 DIAG-SCORE PIC 9(5) VALUE 0.
       01 UP-SCORE PIC 9(5) VALUE 0.
       01 LEFT-SCORE PIC 9(5) VALUE 0.

       01 EOF-FLAG PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-INPUT-SEQUENCES.
           PERFORM COMPUTE-LOCAL-ALIGNMENT.
           PERFORM WRITE-RESULT.
           STOP RUN.

       READ-INPUT-SEQUENCES.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO SEQUENCE1.
           READ INPUT-FILE INTO SEQUENCE2.
           CLOSE INPUT-FILE.

           COMPUTE SEQ1-LENGTH = FUNCTION LENGTH(SEQUENCE1).
           COMPUTE SEQ2-LENGTH = FUNCTION LENGTH(SEQUENCE2).

       COMPUTE-LOCAL-ALIGNMENT.
           PERFORM INITIALIZE-MATRIX.
           PERFORM FILL-MATRIX.
           PERFORM FIND-MAX-SCORE.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SEQ2-LENGTH
               MOVE 0 TO SCORE-CELLS(I)
               MOVE 0 TO SCORE-CELLS-2(I)
               MOVE 0 TO SCORE-CELLS-3(I)
           END-PERFORM.

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SEQ1-LENGTH
               MOVE 0 TO SCORE-CELLS-2(I)
               MOVE 0 TO SCORE-CELLS-3(I)
           END-PERFORM.

       FILL-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SEQ1-LENGTH
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > SEQ2-LENGTH
                   PERFORM COMPUTE-CELL-SCORE
               END-PERFORM
           END-PERFORM.

       COMPUTE-CELL-SCORE.
           IF SEQUENCE1(I:1) = SEQUENCE2(J:1)
               MOVE 1 TO MATCH-SCORE
               MOVE -1 TO MISMATCH-SCORE
           ELSE
               MOVE -1 TO MATCH-SCORE
               MOVE -1 TO MISMATCH-SCORE
           END-IF.

           COMPUTE DIAG-SCORE = SCORE-CELLS-2(J-1) + MATCH-SCORE.
           COMPUTE UP-SCORE = SCORE-CELLS-3(J) - GAP-OPEN - GAP-EXTEND.
           COMPUTE LEFT-SCORE = SCORE-CELLS(J-1) - GAP-OPEN - GAP-EXTEND.

           COMPUTE TEMP-SCORE = FUNCTION MAX(DIAG-SCORE, UP-SCORE, LEFT-SCORE).
           MOVE TEMP-SCORE TO SCORE-CELLS(J).

           COMPUTE DIAG-SCORE = SCORE-CELLS-2(J-1) + MATCH-SCORE.
           COMPUTE UP-SCORE = SCORE-CELLS-3(J) - GAP-OPEN.
           COMPUTE LEFT-SCORE = SCORE-CELLS(J-1) - GAP-OPEN.

           COMPUTE TEMP-SCORE = FUNCTION MAX(DIAG-SCORE, UP-SCORE, LEFT-SCORE).
           MOVE TEMP-SCORE TO SCORE-CELLS-2(J).

           COMPUTE DIAG-SCORE = SCORE-CELLS-2(J-1) + MATCH-SCORE.
           COMPUTE UP-SCORE = SCORE-CELLS-3(J) - GAP-OPEN.
           COMPUTE LEFT-SCORE = SCORE-CELLS(J-1) - GAP-OPEN.

           COMPUTE TEMP-SCORE = FUNCTION MAX(DIAG-SCORE, UP-SCORE, LEFT-SCORE).
           MOVE TEMP-SCORE TO SCORE-CELLS-3(J).

       FIND-MAX-SCORE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > SEQ2-LENGTH
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > SEQ1-LENGTH
                   IF SCORE-CELLS(J) > MAX-SCORE
                       MOVE SCORE-CELLS(J) TO MAX-SCORE
                       MOVE I TO MAX-SCORE-POS
                       MOVE J TO MAX-SCORE-POS2
                   END-IF
               END-PERFORM
           END-PERFORM.

       WRITE-RESULT.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE MAX-SCORE TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.
           CLOSE OUTPUT-FILE.
```

## Explanation

This COBOL program solves the local alignment problem with affine gap penalties by:

1. **Reading Input**: Reading two sequences from an input file
2. **Matrix Initialization**: Creating three matrices to track different gap states:
   - Main score matrix
   - Gap open matrix
   - Gap extension matrix
3. **Dynamic Programming**: Computing scores using affine gap penalties:
   - Gap opening penalty: -11
   - Gap extension penalty: -1
4. **Score Calculation**: For each cell, calculating the maximum score from:
   - Match/mismatch (diagonal)
   - Gap opening (up or left)
   - Gap extension (up or left)
5. **Result**: Finding and outputting the maximum score

## Key Features

- Uses three-dimensional dynamic programming approach
- Implements affine gap penalties correctly
- Handles local alignment (scores can be negative, but we track maximum)
- Properly initializes matrices and handles boundary conditions
- Outputs result to a file

## Usage

To run this program:
1. Create an input file named "input.txt" with two sequences (one per line)
2. Compile and run the COBOL program
3. Output will be written to "output.txt"

The program handles the specific parameters from Rosalind's problem: gap opening penalty of 11 and gap extension penalty of 1.

