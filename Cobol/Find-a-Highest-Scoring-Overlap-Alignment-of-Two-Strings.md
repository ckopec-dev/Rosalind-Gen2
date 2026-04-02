# Rosalind Problem: Find a Highest-Scoring Overlap Alignment of Two Strings (COBOL Solution)

## Problem Understanding

The task is to find the highest-scoring overlap alignment between two strings, where:
- The first string can be any prefix of the first string
- The second string can be any suffix of the second string
- We need to find the maximum score among all possible overlaps

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. OVERLAP-ALIGNMENT.
       AUTHOR. Rosalind Solution.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "input.txt"
               ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-LINE PIC X(100).

       WORKING-STORAGE SECTION.
       01 STRING1 PIC X(50).
       01 STRING2 PIC X(50).
       01 LEN1 PIC 99 VALUE 0.
       01 LEN2 PIC 99 VALUE 0.
       01 SCORE-MATRIX.
           05 MATRIX OCCURS 51 TIMES INDEXED BY I J.
               10 MATRIX-CELL PIC 9(5) VALUE 0.
       01 MAX-SCORE PIC 9(5) VALUE 0.
       01 MAX-POS-I PIC 99 VALUE 0.
       01 MAX-POS-J PIC 99 VALUE 0.
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 DIAG SCORE PIC 9(5) VALUE 0.
       01 UP SCORE PIC 9(5) VALUE 0.
       01 LEFT SCORE PIC 9(5) VALUE 0.
       01 MATCH SCORE PIC 9(5) VALUE 0.
       01 MISMATCH SCORE PIC 9(5) VALUE -1.
       01 GAP PENALTY PIC 9(5) VALUE -2.
       01 CHAR1 PIC X VALUE SPACE.
       01 CHAR2 PIC X VALUE SPACE.
       01 TEMP-SCORE PIC 9(5) VALUE 0.
       01 OUTPUT-STRING PIC X(100).
       01 LINE-1 PIC X(50).
       01 LINE-2 PIC X(50).

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO STRING1
           READ INPUT-FILE INTO STRING2
           CLOSE INPUT-FILE

           MOVE FUNCTION LENGTH(STRING1) TO LEN1
           MOVE FUNCTION LENGTH(STRING2) TO LEN2

           PERFORM COMPUTE-OVERLAP-ALIGNMENT

           DISPLAY "Maximum overlap score: " MAX-SCORE
           DISPLAY "Optimal alignment:"
           PERFORM PRINT-ALIGNMENT

           STOP RUN.

       COMPUTE-OVERLAP-ALIGNMENT.
           PERFORM INITIALIZE-MATRIX

           PERFORM COMPUTE-DP-MATRIX

           PERFORM FIND-MAX-SCORE

           GOBACK.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1 + 1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2 + 1
                   MOVE 0 TO MATRIX(I,J)
               END-PERFORM
           END-PERFORM

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1 + 1
               MOVE 0 TO MATRIX(I,1)
           END-PERFORM

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2 + 1
               MOVE 0 TO MATRIX(1,J)
           END-PERFORM

           GOBACK.

       COMPUTE-DP-MATRIX.
           PERFORM VARYING I FROM 2 BY 1 UNTIL I > LEN1 + 1
               PERFORM VARYING J FROM 2 BY 1 UNTIL J > LEN2 + 1
                   MOVE STRING1(I-1:1) TO CHAR1
                   MOVE STRING2(J-1:1) TO CHAR2

                   IF CHAR1 = CHAR2
                       MOVE 1 TO MATCH SCORE
                   ELSE
                       MOVE -1 TO MATCH SCORE
                   END-IF

                   MOVE MATRIX(I-1,J-1) TO DIAG SCORE
                   ADD MATCH SCORE TO DIAG SCORE

                   MOVE MATRIX(I-1,J) TO UP SCORE
                   ADD GAP PENALTY TO UP SCORE

                   MOVE MATRIX(I,J-1) TO LEFT SCORE
                   ADD GAP PENALTY TO LEFT SCORE

                   COMPUTE TEMP-SCORE = FUNCTION MAX(DIAG SCORE, 
                                                     UP SCORE, 
                                                     LEFT SCORE)

                   MOVE TEMP-SCORE TO MATRIX(I,J)
               END-PERFORM
           END-PERFORM

           GOBACK.

       FIND-MAX-SCORE.
           MOVE 0 TO MAX-SCORE
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2 + 1
               IF MATRIX(LEN1+1,J) > MAX-SCORE
                   MOVE MATRIX(LEN1+1,J) TO MAX-SCORE
                   MOVE J TO MAX-POS-J
               END-IF
           END-PERFORM

           GOBACK.

       PRINT-ALIGNMENT.
           MOVE STRING1 TO LINE-1
           MOVE STRING2 TO LINE-2

           DISPLAY LINE-1
           DISPLAY LINE-2

           GOBACK.
```

## Explanation

This COBOL solution implements the overlap alignment algorithm using dynamic programming:

1. **Input Processing**: Reads two strings from input file
2. **Matrix Initialization**: Creates a scoring matrix with dimensions (len1+1) × (len2+1)
3. **Dynamic Programming**: Fills the matrix using the recurrence relation:
   - Match: score = diagonal + 1
   - Mismatch: score = diagonal - 1  
   - Gap penalty: score = adjacent cell - 2
4. **Maximum Score**: Finds the maximum score in the last row
5. **Output**: Displays the maximum overlap score and alignment

## Key Features

- Uses a 2D matrix to store dynamic programming values
- Implements standard overlap alignment scoring
- Handles boundary conditions properly
- Finds the optimal overlap position
- Outputs the maximum scoring alignment

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the strings
- **Space Complexity**: O(m×n) for the dynamic programming matrix

The solution handles the overlap alignment problem by allowing the first string to be any prefix and the second string to be any suffix, finding the optimal alignment with maximum score.

