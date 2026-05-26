# Maximizing the Gap Symbols of an Optimal Alignment in COBOL

This problem involves finding an optimal alignment between two sequences that maximizes the number of gap symbols. I'll solve this using dynamic programming with a modified scoring scheme.

## Problem Understanding

We need to find an optimal alignment between two sequences that maximizes gap symbols, which means we want to maximize the number of gaps in the alignment.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAX_GAP_ALIGNMENT.
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
       01 SEQ1 PIC X(50).
       01 SEQ2 PIC X(50).
       01 LEN1 PIC 99 VALUE 0.
       01 LEN2 PIC 99 VALUE 0.
       01 I PIC 99 VALUE 0.
       01 J PIC 99 VALUE 0.
       01 MAX-GAPS PIC 99 VALUE 0.
       01 SCORE PIC 99 VALUE 0.
       01 GAP-PENALTY PIC 99 VALUE -1.
       01 MATCH-PENALTY PIC 99 VALUE 0.
       01 MISMATCH-PENALTY PIC 99 VALUE -1.

       01 DP-MATRIX.
           05 DP-ROW OCCURS 50 TIMES.
               10 DP-CELL OCCURS 50 TIMES PIC 99.

       01 ALIGNMENT-1 PIC X(100).
       01 ALIGNMENT-2 PIC X(100).
       01 ALIGNMENT-LEN PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM READ-INPUT.
           PERFORM COMPUTE-ALIGNMENT.
           PERFORM OUTPUT-RESULTS.
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE.
           READ INPUT-FILE INTO SEQ1.
           READ INPUT-FILE INTO SEQ2.
           CLOSE INPUT-FILE.
           
           COMPUTE LEN1 = FUNCTION LENGTH(SEQ1).
           COMPUTE LEN2 = FUNCTION LENGTH(SEQ2).

       COMPUTE-ALIGNMENT.
           PERFORM INITIALIZE-DP-MATRIX.
           PERFORM FILL-DP-MATRIX.
           PERFORM TRACEBACK-ALIGNMENT.

       INITIALIZE-DP-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE 0 TO DP-CELL(I, 0)
           END-PERFORM.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE 0 TO DP-CELL(0, J)
           END-PERFORM.
           
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               MOVE -I TO DP-CELL(I, 0)
           END-PERFORM.
           
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
               MOVE -J TO DP-CELL(0, J)
           END-PERFORM.

       FILL-DP-MATRIX.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > LEN1
               PERFORM VARYING J FROM 1 BY 1 UNTIL J > LEN2
                   IF SEQ1(I:1) = SEQ2(J:1)
                       COMPUTE SCORE = MATCH-PENALTY
                   ELSE
                       COMPUTE SCORE = MISMATCH-PENALTY
                   END-IF
                   
                   COMPUTE DP-CELL(I, J) = MAX(
                       DP-CELL(I-1, J) + GAP-PENALTY,
                       DP-CELL(I, J-1) + GAP-PENALTY,
                       DP-CELL(I-1, J-1) + SCORE
                   )
               END-PERFORM
           END-PERFORM.

       TRACEBACK-ALIGNMENT.
           MOVE LEN1 TO I.
           MOVE LEN2 TO J.
           MOVE 0 TO ALIGNMENT-LEN.
           
           PERFORM UNTIL I = 0 AND J = 0
               IF I > 0 AND J > 0
                   IF SEQ1(I:1) = SEQ2(J:1)
                       COMPUTE SCORE = MATCH-PENALTY
                   ELSE
                       COMPUTE SCORE = MISMATCH-PENALTY
                   END-IF
                   
                   IF DP-CELL(I, J) = DP-CELL(I-1, J-1) + SCORE
                       SUBTRACT 1 FROM I
                       SUBTRACT 1 FROM J
                       MOVE SEQ1(I+1:1) TO ALIGNMENT-1(ALIGNMENT-LEN+1:1)
                       MOVE SEQ2(J+1:1) TO ALIGNMENT-2(ALIGNMENT-LEN+1:1)
                       ADD 1 TO ALIGNMENT-LEN
                   ELSE IF DP-CELL(I, J) = DP-CELL(I-1, J) + GAP-PENALTY
                       SUBTRACT 1 FROM I
                       MOVE '-' TO ALIGNMENT-1(ALIGNMENT-LEN+1:1)
                       MOVE SEQ2(J:1) TO ALIGNMENT-2(ALIGNMENT-LEN+1:1)
                       ADD 1 TO ALIGNMENT-LEN
                   ELSE
                       SUBTRACT 1 FROM J
                       MOVE SEQ1(I:1) TO ALIGNMENT-1(ALIGNMENT-LEN+1:1)
                       MOVE '-' TO ALIGNMENT-2(ALIGNMENT-LEN+1:1)
                       ADD 1 TO ALIGNMENT-LEN
                   END-IF
               ELSE IF I > 0
                   SUBTRACT 1 FROM I
                   MOVE SEQ1(I:1) TO ALIGNMENT-1(ALIGNMENT-LEN+1:1)
                   MOVE '-' TO ALIGNMENT-2(ALIGNMENT-LEN+1:1)
                   ADD 1 TO ALIGNMENT-LEN
               ELSE
                   SUBTRACT 1 FROM J
                   MOVE '-' TO ALIGNMENT-1(ALIGNMENT-LEN+1:1)
                   MOVE SEQ2(J:1) TO ALIGNMENT-2(ALIGNMENT-LEN+1:1)
                   ADD 1 TO ALIGNMENT-LEN
               END-IF
           END-PERFORM.

       OUTPUT-RESULTS.
           OPEN OUTPUT OUTPUT-FILE.
           MOVE ALIGNMENT-LEN TO MAX-GAPS.
           WRITE OUTPUT-LINE FROM MAX-GAPS.
           CLOSE OUTPUT-FILE.

       MAX FUNCTION.
           IF A > B
               RETURN A
           ELSE
               RETURN B
           END-IF.
```

## Key Points of the Solution

1. **Dynamic Programming Approach**: Uses a 2D matrix to store optimal alignment scores
2. **Gap Maximization**: The algorithm is modified to maximize gaps rather than minimize them
3. **Traceback Process**: Reconstructs the alignment by backtracking through the DP matrix
4. **Scoring System**: 
   - Match: 0 points
   - Mismatch: -1 points  
   - Gap: -1 points

## Algorithm Steps

1. **Initialize**: Set up DP matrix with base cases
2. **Fill Matrix**: Compute optimal scores using recurrence relation
3. **Traceback**: Reconstruct alignment by following optimal path
4. **Output**: Return the maximum number of gaps

## Time and Space Complexity

- **Time Complexity**: O(m×n) where m and n are sequence lengths
- **Space Complexity**: O(m×n) for the DP matrix

This solution finds an optimal alignment that maximizes the number of gap symbols while maintaining a valid alignment between the two sequences.

