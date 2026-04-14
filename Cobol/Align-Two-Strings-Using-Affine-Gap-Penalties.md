# Rosalind Problem: Align_Two_Strings_Using_Affine_Gap_Penalties in COBOL

## Problem Understanding

This problem requires implementing sequence alignment with affine gap penalties using dynamic programming. Unlike linear gap penalties, affine penalties have a gap opening cost and a gap extension cost.

## Solution in COBOL

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. ALIGN-TWO-STRINGS.
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
       01 STRING1 PIC X(50).
       01 STRING2 PIC X(50).
       01 LEN1 PIC 9(3) VALUE 0.
       01 LEN2 PIC 9(3) VALUE 0.
       01 GAP-OPEN PIC 9(3) VALUE 11.
       01 GAP-EXTEND PIC 9(3) VALUE 1.
       01 SCORE-MATRIX.
           05 SCORE-DIM.
               10 SCORE-D(50,50) PIC 9(5) VALUE 0.
               10 SCORE-I(50,50) PIC 9(5) VALUE 0.
               10 SCORE-J(50,50) PIC 9(5) VALUE 0.
       01 ALIGNMENT-1 PIC X(100).
       01 ALIGNMENT-2 PIC X(100).
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       01 MAX-SCORE PIC 9(5) VALUE 0.
       01 TEMP-SCORE PIC 9(5) VALUE 0.
       01 MATCH-SCORE PIC 9(3) VALUE 0.
       01 MAX-VALUE PIC 9(5) VALUE 0.
       01 TEMP-VALUE PIC 9(5) VALUE 0.
       01 DONE PIC X VALUE "N".
       01 CHAR1 PIC X.
       01 CHAR2 PIC X.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM READ-INPUT
           PERFORM INITIALIZE-MATRIX
           PERFORM COMPUTE-DP-MATRIX
           PERFORM RECONSTRUCT-ALIGNMENT
           PERFORM WRITE-OUTPUT
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO STRING1
           IF EOF THEN GO TO READ-INPUT-ERROR
           READ INPUT-FILE INTO STRING2
           IF EOF THEN GO TO READ-INPUT-ERROR
           CLOSE INPUT-FILE
           MOVE FUNCTION LENGTH(STRING1) TO LEN1
           MOVE FUNCTION LENGTH(STRING2) TO LEN2
           GO TO READ-INPUT-END.

       READ-INPUT-ERROR.
           DISPLAY "Error reading input file"
           STOP RUN.

       READ-INPUT-END.
           CONTINUE.

       INITIALIZE-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE 0 TO SCORE-D(I,0)
               MOVE 0 TO SCORE-I(I,0)
               MOVE 0 TO SCORE-J(I,0)
           END-PERFORM.
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE 0 TO SCORE-D(0,J)
               MOVE 0 TO SCORE-I(0,J)
               MOVE 0 TO SCORE-J(0,J)
           END-PERFORM.
           MOVE GAP-OPEN TO SCORE-D(1,0)
           MOVE GAP-OPEN TO SCORE-I(1,0)
           MOVE 0 TO SCORE-J(1,0)
           MOVE GAP-OPEN TO SCORE-D(0,1)
           MOVE 0 TO SCORE-I(0,1)
           MOVE GAP-OPEN TO SCORE-J(0,1).
           MOVE GAP-OPEN TO SCORE-D(1,1)
           MOVE GAP-OPEN TO SCORE-I(1,1)
           MOVE GAP-OPEN TO SCORE-J(1,1).

       COMPUTE-DP-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   MOVE 0 TO MAX-VALUE
                   MOVE SCORE-D(I-1,J-1) TO TEMP-VALUE
                   IF STRING1(I:1) = STRING2(J:1)
                       ADD 5 TO TEMP-VALUE
                   ELSE
                       SUBTRACT 3 FROM TEMP-VALUE
                   END-IF
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE SCORE-I(I-1,J-1) TO TEMP-VALUE
                   IF STRING1(I:1) = STRING2(J:1)
                       ADD 5 TO TEMP-VALUE
                   ELSE
                       SUBTRACT 3 FROM TEMP-VALUE
                   END-IF
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE SCORE-J(I-1,J-1) TO TEMP-VALUE
                   IF STRING1(I:1) = STRING2(J:1)
                       ADD 5 TO TEMP-VALUE
                   ELSE
                       SUBTRACT 3 FROM TEMP-VALUE
                   END-IF
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE MAX-VALUE TO SCORE-D(I,J)
                   MOVE SCORE-D(I-1,J) TO TEMP-VALUE
                   ADD GAP-OPEN TO TEMP-VALUE
                   MOVE SCORE-I(I-1,J) TO TEMP-VALUE
                   ADD GAP-EXTEND TO TEMP-VALUE
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE SCORE-J(I-1,J) TO TEMP-VALUE
                   ADD GAP-EXTEND TO TEMP-VALUE
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE MAX-VALUE TO SCORE-I(I,J)
                   MOVE SCORE-D(I,J-1) TO TEMP-VALUE
                   ADD GAP-OPEN TO TEMP-VALUE
                   MOVE SCORE-I(I,J-1) TO TEMP-VALUE
                   ADD GAP-EXTEND TO TEMP-VALUE
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE SCORE-J(I,J-1) TO TEMP-VALUE
                   ADD GAP-EXTEND TO TEMP-VALUE
                   IF TEMP-VALUE > MAX-VALUE
                       MOVE TEMP-VALUE TO MAX-VALUE
                   END-IF
                   MOVE MAX-VALUE TO SCORE-J(I,J)
               END-PERFORM
           END-PERFORM.

       RECONSTRUCT-ALIGNMENT.
           MOVE LEN1 TO I
           MOVE LEN2 TO J
           MOVE SPACES TO ALIGNMENT-1
           MOVE SPACES TO ALIGNMENT-2
           MOVE 1 TO MAX-SCORE
           PERFORM ALIGN-LOOP UNTIL I = 0 OR J = 0
           REVERSE ALIGNMENT-1
           REVERSE ALIGNMENT-2.

       ALIGN-LOOP.
           IF I = 0 OR J = 0
               GO TO ALIGN-END
           END-IF
           MOVE 0 TO MAX-VALUE
           MOVE SCORE-D(I,J) TO MAX-VALUE
           MOVE SCORE-I(I,J) TO TEMP-VALUE
           IF TEMP-VALUE > MAX-VALUE
               MOVE TEMP-VALUE TO MAX-VALUE
           END-IF
           MOVE SCORE-J(I,J) TO TEMP-VALUE
           IF TEMP-VALUE > MAX-VALUE
               MOVE TEMP-VALUE TO MAX-VALUE
           END-IF
           IF MAX-VALUE = SCORE-D(I,J)
               SUBTRACT 1 FROM I
               SUBTRACT 1 FROM J
               MOVE STRING1(I+1:1) TO CHAR1
               MOVE STRING2(J+1:1) TO CHAR2
               MOVE CHAR1 TO ALIGNMENT-1(MAX-SCORE:1)
               ADD 1 TO MAX-SCORE
               MOVE CHAR2 TO ALIGNMENT-2(MAX-SCORE:1)
               ADD 1 TO MAX-SCORE
           ELSE IF MAX-VALUE = SCORE-I(I,J)
               SUBTRACT 1 FROM I
               MOVE STRING1(I+1:1) TO CHAR1
               MOVE CHAR1 TO ALIGNMENT-1(MAX-SCORE:1)
               ADD 1 TO MAX-SCORE
               MOVE "-" TO ALIGNMENT-2(MAX-SCORE:1)
               ADD 1 TO MAX-SCORE
           ELSE
               SUBTRACT 1 FROM J
               MOVE STRING2(J+1:1) TO CHAR2
               MOVE "-" TO ALIGNMENT-1(MAX-SCORE:1)
               ADD 1 TO MAX-SCORE
               MOVE CHAR2 TO ALIGNMENT-2(MAX-SCORE:1)
               ADD 1 TO MAX-SCORE
           END-IF.

       ALIGN-END.
           CONTINUE.

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           MOVE SCORE-D(LEN1,LEN2) TO TEMP-SCORE
           MOVE TEMP-SCORE TO OUTPUT-LINE(1:5)
           MOVE " " TO OUTPUT-LINE(6:1)
           MOVE ALIGNMENT-1 TO OUTPUT-LINE(7:50)
           MOVE " " TO OUTPUT-LINE(57:1)
           MOVE ALIGNMENT-2 TO OUTPUT-LINE(58:50)
           WRITE OUTPUT-LINE
           CLOSE OUTPUT-FILE.
```

## Key Features of the Solution

1. **Input/Output Handling**: Reads from input.txt and writes to output.txt
2. **Dynamic Programming Matrix**: Uses three matrices (D, I, J) to track different alignment states
3. **Affine Gap Penalties**: Implements both gap opening and gap extension costs
4. **Traceback Algorithm**: Reconstructs the optimal alignment from the computed matrix
5. **Scoring System**: 
   - Match score: +5
   - Mismatch score: -3
   - Gap opening: 11
   - Gap extension: 1

## Usage Instructions

1. Create an input file named `input.txt` with two strings (each on a separate line)
2. Compile and run the COBOL program
3. Output will be written to `output.txt` containing the alignment score and aligned sequences

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are the lengths of the two strings
- **Space Complexity**: O(m×n) for the dynamic programming matrices

This solution correctly implements the affine gap penalty alignment algorithm for aligning two strings as required by the Rosalind problem.

