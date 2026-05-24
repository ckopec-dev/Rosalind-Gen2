# Rosalind Problem: Sorting_by_Reversals in COBOL

## Problem Understanding

The Sorting_by_Reversals problem involves finding the minimum number of reversals needed to transform one permutation into another. In this context, we're looking for the minimum number of reversals to sort a permutation.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SORTING-BY-REVERSALS.
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
       01 PERMUTATION.
           05 PERM1 OCCURS 10 TIMES PIC 9.
           05 PERM2 OCCURS 10 TIMES PIC 9.
           05 TEMP-PERM OCCURS 10 TIMES PIC 9.
       
       01 REVERSAL-COUNT PIC 99 VALUE 0.
       01 CURRENT-INDEX PIC 99 VALUE 1.
       01 TARGET-INDEX PIC 99 VALUE 1.
       01 TEMP-VALUE PIC 99 VALUE 0.
       01 SWAP-INDEX PIC 99 VALUE 0.
       01 FOUND-FLAG PIC X VALUE "N".
       01 LINE-COUNT PIC 99 VALUE 0.
       01 I PIC 99 VALUE 1.
       01 J PIC 99 VALUE 1.
       01 K PIC 99 VALUE 1.
       01 N PIC 99 VALUE 0.
       01 MAX-REVERSALS PIC 99 VALUE 0.
       01 DONE-FLAG PIC X VALUE "N".
       01 EOF-FLAG PIC X VALUE "N".
       01 LINE-LENGTH PIC 99 VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           OPEN INPUT INPUT-FILE
           OPEN OUTPUT OUTPUT-FILE

           PERFORM READ-INPUT-FILE
           PERFORM CALCULATE-MIN-REVERSALS
           PERFORM WRITE-OUTPUT

           CLOSE INPUT-FILE
           CLOSE OUTPUT-FILE
           STOP RUN.

       READ-INPUT-FILE.
           READ INPUT-FILE INTO INPUT-LINE
               AT END MOVE "Y" TO EOF-FLAG
           END-READ.

           IF EOF-FLAG = "N"
               PERFORM PARSE-LINE
           END-IF.

       PARSE-LINE.
           MOVE 0 TO N
           MOVE 1 TO CURRENT-INDEX
           MOVE 1 TO I

           PERFORM UNTIL CURRENT-INDEX > 100
               IF INPUT-LINE(CURRENT-INDEX:1) = " "
                   ADD 1 TO I
                   ADD 1 TO CURRENT-INDEX
               ELSE
                   COMPUTE TEMP-VALUE = 10 * TEMP-VALUE + 
                       (INPUT-LINE(CURRENT-INDEX:1) - "0")
                   ADD 1 TO CURRENT-INDEX
               END-IF

               IF INPUT-LINE(CURRENT-INDEX:1) = " " OR 
                  CURRENT-INDEX > 100
                   MOVE TEMP-VALUE TO PERM1(I)
                   MOVE 0 TO TEMP-VALUE
                   IF I > N
                       MOVE I TO N
                   END-IF
                   ADD 1 TO I
               END-IF
           END-PERFORM.

           PERFORM READ-INPUT-FILE.

       CALCULATE-MIN-REVERSALS.
           MOVE 0 TO REVERSAL-COUNT
           MOVE 1 TO CURRENT-INDEX

           PERFORM UNTIL CURRENT-INDEX > N OR REVERSAL-COUNT > 100
               MOVE "N" TO FOUND-FLAG
               MOVE 1 TO TARGET-INDEX

               PERFORM UNTIL TARGET-INDEX > N OR FOUND-FLAG = "Y"
                   IF PERM1(TARGET-INDEX) = TARGET-INDEX
                       ADD 1 TO TARGET-INDEX
                   ELSE
                       MOVE "Y" TO FOUND-FLAG
                       MOVE TARGET-INDEX TO SWAP-INDEX
                   END-IF
               END-PERFORM

               IF FOUND-FLAG = "Y"
                   PERFORM REVERSE-SEGMENT
                   ADD 1 TO REVERSAL-COUNT
               END-IF

               ADD 1 TO CURRENT-INDEX
           END-PERFORM.

       REVERSE-SEGMENT.
           MOVE 1 TO I
           MOVE SWAP-INDEX TO J

           PERFORM UNTIL I >= J
               MOVE PERM1(I) TO TEMP-VALUE
               MOVE PERM1(J) TO PERM1(I)
               MOVE TEMP-VALUE TO PERM1(J)
               ADD 1 TO I
               SUBTRACT 1 FROM J
           END-PERFORM.

       WRITE-OUTPUT.
           MOVE REVERSAL-COUNT TO TEMP-VALUE
           STRING "Minimum reversals needed: " TEMP-VALUE
               INTO OUTPUT-LINE
           WRITE OUTPUT-LINE.

       END PROGRAM SORTING-BY-REVERSALS.
```

## Key Components Explanation

### 1. **Data Division**
- **File Section**: Defines input/output files for reading permutation data
- **Working Storage**: Contains permutation arrays, counters, and temporary variables

### 2. **Algorithm Approach**
The solution uses a greedy approach:
- Find the first element that's not in its correct position
- Reverse the segment from the beginning to that element's current position
- Continue until the permutation is sorted

### 3. **Main Steps**
1. **Read Input**: Parse the permutation from input file
2. **Calculate Reversals**: Apply greedy algorithm to find minimum reversals
3. **Write Output**: Display the result

### 4. **Key Variables**
- `PERM1`: Current permutation being sorted
- `REVERSAL-COUNT`: Number of reversals performed
- `SWAP-INDEX`: Position of element that needs to be moved
- `FOUND-FLAG`: Indicates if misplaced element is found

## Usage Notes

1. **Input Format**: Expected to be a single line with space-separated integers
2. **Output**: Displays the minimum number of reversals needed
3. **Limitation**: The solution assumes a maximum of 100 reversals for safety
4. **Efficiency**: Uses greedy approach which works well for this problem type

This COBOL implementation provides a structured approach to solving the Sorting_by_Reversals problem using classic algorithmic techniques.

