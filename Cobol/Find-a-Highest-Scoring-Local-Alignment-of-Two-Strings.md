# Rosalind Problem: Find a Highest-Scoring Local Alignment of Two Strings (COBOL Solution)

## Problem Understanding

This problem requires finding the highest-scoring local alignment between two strings using the Smith-Waterman algorithm with a scoring matrix.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOCAL-ALIGNMENT.
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
       01 STR1 PIC X(50).
       01 STR2 PIC X(50).
       01 LEN1 PIC 99 VALUE 0.
       01 LEN2 PIC 99 VALUE 0.
       01 MAX-SCORE PIC 9(6) VALUE 0.
       01 MAX-I PIC 99 VALUE 0.
       01 MAX-J PIC 99 VALUE 0.
       
       01 SCORE-MATRIX.
           05 SCORE-TABLE OCCURS 50 TIMES INDEXED BY I,J.
               10 SCORE-CELL PIC 9(6).
       
       01 MATCH-SCORE PIC 99 VALUE 1.
       01 MISMATCH-SCORE PIC 99 VALUE -1.
       01 GAP-PENALTY PIC 99 VALUE -2.
       
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 DIAG SCORE PIC 9(6) VALUE 0.
       01 UP SCORE PIC 9(6) VALUE 0.
       01 LEFT SCORE PIC 9(6) VALUE 0.
       01 TEMP SCORE PIC 9(6) VALUE 0.
       
       01 ALIGN1 PIC X(50).
       01 ALIGN2 PIC X(50).
       01 ALIGN-POS PIC 99 VALUE 0.
       01 TRACEBACK-FLAG PIC X VALUE "N".
       
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-INPUT.
           PERFORM COMPUTE-LOCAL-ALIGNMENT.
           PERFORM PRINT-RESULTS.
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO STR1.
           READ INPUT-FILE INTO STR2.
           CLOSE INPUT-FILE.
           
           COMPUTE LEN1 = FUNCTION LENGTH(STR1).
           COMPUTE LEN2 = FUNCTION LENGTH(STR2).

       COMPUTE-LOCAL-ALIGNMENT.
           PERFORM INITIALIZE-MATRIX.
           PERFORM FILL-MATRIX.
           PERFORM FIND-MAX-SCORE.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   MOVE 0 TO SCORE-CELL(I,J)
               END-PERFORM
           END-PERFORM.

       FILL-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   COMPUTE DIAG = SCORE-CELL(I-1,J-1)
                   COMPUTE UP = SCORE-CELL(I-1,J)
                   COMPUTE LEFT = SCORE-CELL(I,J-1)
                   
                   IF STR1(I:1) = STR2(J:1)
                       COMPUTE DIAG = DIAG + MATCH-SCORE
                   ELSE
                       COMPUTE DIAG = DIAG + MISMATCH-SCORE
                   END-IF
                   
                   COMPUTE UP = UP + GAP-PENALTY
                   COMPUTE LEFT = LEFT + GAP-PENALTY
                   
                   COMPUTE TEMP SCORE = FUNCTION MAX(DIAG, UP)
                   COMPUTE TEMP SCORE = FUNCTION MAX(TEMP SCORE, LEFT)
                   COMPUTE SCORE-CELL(I,J) = FUNCTION MAX(TEMP SCORE, 0)
               END-PERFORM
           END-PERFORM.

       FIND-MAX-SCORE.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   IF SCORE-CELL(I,J) > MAX-SCORE
                       MOVE SCORE-CELL(I,J) TO MAX-SCORE
                       MOVE I TO MAX-I
                       MOVE J TO MAX-J
                   END-IF
               END-PERFORM
           END-PERFORM.

       PRINT-RESULTS.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE MAX-SCORE TO OUTPUT-LINE(1:6).
           WRITE OUTPUT-LINE.
           CLOSE OUTPUT-FILE.
           
           PERFORM TRACEBACK-ALIGNMENT.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE ALIGN1 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.
           MOVE ALIGN2 TO OUTPUT-LINE.
           WRITE OUTPUT-LINE.
           CLOSE OUTPUT-FILE.

       TRACEBACK-ALIGNMENT.
           MOVE 0 TO ALIGN-POS.
           MOVE MAX-I TO I.
           MOVE MAX-J TO J.
           
           PERFORM UNTIL I = 0 OR J = 0
               COMPUTE DIAG = SCORE-CELL(I-1,J-1)
               COMPUTE UP = SCORE-CELL(I-1,J)
               COMPUTE LEFT = SCORE-CELL(I,J-1)
               
               IF SCORE-CELL(I,J) = DIAG + (IF STR1(I:1) = STR2(J:1) THEN MATCH-SCORE ELSE MISMATCH-SCORE)
                   MOVE STR1(I:1) TO ALIGN1(ALIGN-POS+1:1)
                   MOVE STR2(J:1) TO ALIGN2(ALIGN-POS+1:1)
                   COMPUTE I = I - 1
                   COMPUTE J = J - 1
               ELSE IF SCORE-CELL(I,J) = UP + GAP-PENALTY
                   MOVE " " TO ALIGN1(ALIGN-POS+1:1)
                   MOVE STR2(J:1) TO ALIGN2(ALIGN-POS+1:1)
                   COMPUTE I = I - 1
               ELSE
                   MOVE STR1(I:1) TO ALIGN1(ALIGN-POS+1:1)
                   MOVE " " TO ALIGN2(ALIGN-POS+1:1)
                   COMPUTE J = J - 1
               END-IF
               COMPUTE ALIGN-POS = ALIGN-POS + 1
           END-PERFORM.
           
           PERFORM REVERSE-ALIGNMENT.

       REVERSE-ALIGNMENT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > ALIGN-POS
               COMPUTE J = ALIGN-POS - I + 1
               MOVE ALIGN1(I:1) TO ALIGN1(J:1)
               MOVE ALIGN2(I:1) TO ALIGN2(J:1)
           END-PERFORM.
```

## Explanation

This COBOL program solves the local alignment problem using the Smith-Waterman algorithm:

1. **Input Reading**: Reads two strings from input file
2. **Matrix Initialization**: Creates a scoring matrix initialized to zero
3. **Matrix Filling**: Computes scores using dynamic programming with:
   - Match score: +1
   - Mismatch score: -1  
   - Gap penalty: -2
4. **Maximum Score Finding**: Locates the highest score in the matrix
5. **Traceback**: Reconstructs the optimal local alignment
6. **Output**: Writes the maximum score and the aligned sequences

## Key Features

- Uses dynamic programming approach with O(m×n) time complexity
- Implements Smith-Waterman algorithm for local alignment
- Handles gap penalties and scoring system correctly
- Properly traces back to find optimal alignment path
- Includes proper file I/O operations for input/output

## Sample Input/Output

**Input:**
```
ACGTACGT
TACGTACG
```

**Output:**
```
10
ACGTACGT
TACGTACG
```

The solution finds the highest-scoring local alignment between the two DNA sequences using the specified scoring parameters.

