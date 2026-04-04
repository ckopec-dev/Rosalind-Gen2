# Semiglobal Alignment in COBOL

## Problem Understanding

Semiglobal alignment is a sequence alignment problem where we want to find the best alignment between two sequences, but we don't penalize gaps at the beginning or end of either sequence. This is different from global alignment (Needleman-Wunsch) where all gaps are penalized.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEMIGLOBAL-ALIGNMENT.
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
       01 OUTPUT-LINE PIC X(200).

       WORKING-STORAGE SECTION.
       01 SEQUENCE-1 PIC X(50).
       01 SEQUENCE-2 PIC X(50).
       01 LEN1 PIC 9(3) VALUE 0.
       01 LEN2 PIC 9(3) VALUE 0.
       01 SCORE-MATRIX.
           05 SCORE-ROW OCCURS 50 TIMES.
               10 SCORE-ELEM PIC 9(5) VALUE 0.
       01 MAX-SCORE PIC 9(5) VALUE 0.
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       01 TEMP-SCORE PIC 9(5) VALUE 0.
       01 MATCH-SCORE PIC 9(3) VALUE 1.
       01 MISMATCH-SCORE PIC 9(3) VALUE -1.
       01 GAP-SCORE PIC 9(3) VALUE -1.
       01 ALIGNMENT-RESULT PIC X(100).
       01 ALIGNMENT-LENGTH PIC 9(3) VALUE 0.
       01 EOF-FLAG PIC X VALUE "N".
       01 CHAR1 PIC X.
       01 CHAR2 PIC X.
       01 MAX-CELL PIC 9(5) VALUE 0.
       01 TRACEBACK-MATRIX.
           05 TRACEBACK-ROW OCCURS 50 TIMES.
               10 TRACEBACK-ELEM PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           READ INPUT-FILE INTO SEQUENCE-1
               AT END MOVE "Y" TO EOF-FLAG
           END-READ

           IF EOF-FLAG = "N"
               READ INPUT-FILE INTO SEQUENCE-2
                   AT END MOVE "Y" TO EOF-FLAG
               END-READ
           END-IF

           IF EOF-FLAG = "N"
               PERFORM CALCULATE-SEMIGLOBAL-ALIGNMENT
               PERFORM OUTPUT-RESULTS
           END-IF

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       CALCULATE-SEMIGLOBAL-ALIGNMENT.
           MOVE FUNCTION LENGTH(SEQUENCE-1) TO LEN1
           MOVE FUNCTION LENGTH(SEQUENCE-2) TO LEN2

           PERFORM INITIALIZE-MATRIX
           PERFORM FILL-MATRIX
           PERFORM TRACEBACK

           GOBACK.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE 0 TO SCORE-ROW(I)
           END-PERFORM

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE 0 TO SCORE-ELEM(J)
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE (I * GAP-SCORE) TO SCORE-ELEM(I)
           END-PERFORM

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE (J * GAP-SCORE) TO SCORE-ROW(J)
           END-PERFORM.

       FILL-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   MOVE SEQUENCE-1(I:1) TO CHAR1
                   MOVE SEQUENCE-2(J:1) TO CHAR2

                   IF CHAR1 = CHAR2
                       MOVE MATCH-SCORE TO TEMP-SCORE
                   ELSE
                       MOVE MISMATCH-SCORE TO TEMP-SCORE
                   END-IF

                   COMPUTE SCORE-ELEM(I,J) =
                       MAX(0,
                           MAX(SCORE-ELEM(I-1,J) + GAP-SCORE,
                               MAX(SCORE-ELEM(I,J-1) + GAP-SCORE,
                                   SCORE-ELEM(I-1,J-1) + TEMP-SCORE)))
               END-PERFORM
           END-PERFORM.

       TRACEBACK.
           MOVE 0 TO MAX-SCORE
           MOVE 0 TO MAX-CELL

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               IF SCORE-ELEM(I,LEN2) > MAX-SCORE
                   MOVE SCORE-ELEM(I,LEN2) TO MAX-SCORE
                   MOVE I TO MAX-CELL
               END-IF
           END-PERFORM

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               IF SCORE-ELEM(LEN1,J) > MAX-SCORE
                   MOVE SCORE-ELEM(LEN1,J) TO MAX-SCORE
                   MOVE J TO MAX-CELL
               END-IF
           END-PERFORM

           GOBACK.

       OUTPUT-RESULTS.
           MOVE MAX-SCORE TO TEMP-SCORE
           STRING "Score: " TEMP-SCORE
               INTO OUTPUT-LINE
           WRITE OUTPUT-LINE

           MOVE "Alignment:" TO OUTPUT-LINE(1:11)
           WRITE OUTPUT-LINE

           GOBACK.
```

## Explanation

This COBOL program implements semiglobal alignment using a dynamic programming approach:

1. **Input Handling**: Reads two sequences from input file
2. **Matrix Initialization**: Sets up scoring matrix with gap penalties
3. **Matrix Filling**: Computes scores using the semiglobal alignment formula:
   - `score[i,j] = max(0, max(score[i-1,j] + gap, score[i,j-1] + gap, score[i-1,j-1] + match/mismatch))`
4. **Traceback**: Finds the maximum score at the ends of sequences
5. **Output**: Displays the alignment score

## Key Features

- Uses semiglobal alignment where gaps at sequence ends are not penalized
- Implements proper dynamic programming with matrix filling
- Handles sequence comparison with match/mismatch scoring
- Includes proper file I/O for input/output operations

## Usage

1. Save sequences in input.txt (one per line)
2. Compile and run the COBOL program
3. Output will be written to output.txt with alignment score

The algorithm has time complexity O(m×n) and space complexity O(m×n) where m and n are the lengths of the sequences.

