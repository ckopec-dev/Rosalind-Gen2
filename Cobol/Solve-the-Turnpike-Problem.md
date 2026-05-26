# Rosalind Problem: Solve_the_Turnpike_Problem in COBOL

## Problem Understanding

The Turnpike Problem asks us to reconstruct a set of points on a line given the distances between all pairs of points. Given a set of distances, we need to find the positions of the points.

## Approach

1. **Input**: A set of distances between pairs of points
2. **Output**: The positions of points on a line
3. **Method**: Use backtracking to place points and verify distances

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TURNPIKE-PROBLEM.
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
       01 N PIC 9(3) VALUE 0.
       01 DISTANCES OCCURS 100 TIMES PIC 9(3).
       01 POINTS OCCURS 100 TIMES PIC 9(3).
       01 DISTANCE-COUNT PIC 9(3) VALUE 0.
       01 MAX-DISTANCE PIC 9(3) VALUE 0.
       01 POINTS-COUNT PIC 9(3) VALUE 0.
       01 I PIC 9(3) VALUE 0.
       01 J PIC 9(3) VALUE 0.
       01 K PIC 9(3) VALUE 0.
       01 L PIC 9(3) VALUE 0.
       01 M PIC 9(3) VALUE 0.
       01 FOUND PIC X VALUE "N".
       01 TEMP-DISTANCE PIC 9(3) VALUE 0.
       01 TEMP-POINT PIC 9(3) VALUE 0.
       01 VALID-SOLUTION PIC X VALUE "N".
       01 EOF-FLAG PIC X VALUE "N".
       01 LINE-READ PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-INPUT
           PERFORM SOLVE-TURNPIKE
           PERFORM WRITE-OUTPUT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-INPUT.
           READ INPUT-FILE INTO INPUT-LINE
               AT END MOVE "Y" TO EOF-FLAG
           END-READ.

           IF EOF-FLAG = "N"
               PERFORM PARSE-INPUT
           END-IF.

       PARSE-INPUT.
           MOVE 0 TO DISTANCE-COUNT
           MOVE 0 TO MAX-DISTANCE

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > 100
               IF INPUT-LINE(I:1) = " "
                   ADD 1 TO DISTANCE-COUNT
                   MOVE FUNCTION NUMVAL(INPUT-LINE(I+1:3)) TO DISTANCES(DISTANCE-COUNT)
                   IF DISTANCES(DISTANCE-COUNT) > MAX-DISTANCE
                       MOVE DISTANCES(DISTANCE-COUNT) TO MAX-DISTANCE
                   END-IF
               ELSE IF INPUT-LINE(I:1) = X"00"
                   EXIT PARAGRAPH
               END-IF
           END-PERFORM.

           COMPUTE N = (DISTANCE-COUNT * 2) / (DISTANCE-COUNT - 1)
           MOVE N TO POINTS-COUNT.

       SOLVE-TURNPIKE.
           MOVE "N" TO VALID-SOLUTION
           MOVE 0 TO POINTS(1)
           MOVE 0 TO POINTS(2)
           MOVE MAX-DISTANCE TO POINTS(N)

           PERFORM BACKTRACK-SEARCH VARYING I FROM 3 BY 1 UNTIL I > N
               IF VALID-SOLUTION = "Y"
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       BACKTRACK-SEARCH.
           IF I > N
               MOVE "Y" TO VALID-SOLUTION
               GO TO BACKTRACK-END
           END-IF.

           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               IF POINTS(J) = 0
                   MOVE J TO TEMP-POINT
                   MOVE TEMP-POINT TO POINTS(I)
                   IF I = N
                       PERFORM VERIFY-SOLUTION
                       IF VALID-SOLUTION = "Y"
                           GO TO BACKTRACK-END
                       END-IF
                   ELSE
                       PERFORM BACKTRACK-SEARCH
                       IF VALID-SOLUTION = "Y"
                           GO TO BACKTRACK-END
                       END-IF
                   END-IF
                   MOVE 0 TO POINTS(I)
               END-IF
           END-PERFORM.

       BACKTRACK-END.
           EXIT.

       VERIFY-SOLUTION.
           MOVE "Y" TO VALID-SOLUTION
           PERFORM VARYING J FROM 1 BY 1 UNTIL J > N
               PERFORM VARYING K FROM J BY 1 UNTIL K > N
                   IF J = K
                       CONTINUE
                   ELSE
                       COMPUTE TEMP-DISTANCE = FUNCTION ABS(POINTS(J) - POINTS(K))
                       PERFORM CHECK-DISTANCE
                       IF FOUND = "N"
                           MOVE "N" TO VALID-SOLUTION
                           GO TO VERIFY-END
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM.

       VERIFY-END.
           EXIT.

       CHECK-DISTANCE.
           MOVE "N" TO FOUND
           PERFORM VARYING L FROM 1 BY 1 UNTIL L > DISTANCE-COUNT
               IF DISTANCES(L) = TEMP-DISTANCE
                   MOVE "Y" TO FOUND
                   EXIT PERFORM
               END-IF
           END-PERFORM.

       WRITE-OUTPUT.
           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               IF I > 1
                   MOVE SPACE TO OUTPUT-LINE(1:1)
               END-IF
               MOVE POINTS(I) TO OUTPUT-LINE(2:3)
           END-PERFORM.

           WRITE OUTPUT-LINE
           MOVE "Y" TO LINE-READ.
```

## Explanation

This COBOL solution implements a backtracking algorithm to solve the Turnpike problem:

1. **Input Reading**: Reads distances from input file
2. **Parsing**: Extracts distances and determines the number of points
3. **Backtracking**: Uses recursive backtracking to try different point placements
4. **Validation**: Verifies that all distances match the given set
5. **Output**: Writes the solution to output file

## Key Features

- **Backtracking Algorithm**: Systematically tries point placements
- **Distance Verification**: Checks if all distances match the input
- **Recursive Solution**: Uses recursion for efficient search
- **File I/O**: Handles input/output operations properly

## Usage

1. Input file should contain distances separated by spaces
2. Output file will contain the reconstructed point positions
3. The algorithm assumes the problem has a valid solution

## Time Complexity

The algorithm has exponential time complexity in the worst case due to backtracking, but typically performs well for small to medium-sized inputs.

