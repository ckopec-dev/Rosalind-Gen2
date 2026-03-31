# Rosalind Problem Quick_Sort in COBOL

## Problem Understanding

Quick Sort is a divide-and-conquer sorting algorithm that works by selecting a 'pivot' element and partitioning the array around it. The goal is to sort an array of integers in ascending order.

## COBOL Solution

```cobol
       IDENTIFICATION DIVISION.
       PROGRAM-ID. QUICK-SORT.
       AUTHOR. Rosalind Problem Solver.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "rosalind_qs.txt"
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
       01 N PIC 9(4) VALUE 0.
       01 ARRAY OCCURS 1000 TIMES PIC 9(6).
       01 I PIC 9(4) VALUE 0.
       01 J PIC 9(4) VALUE 0.
       01 PIVOT PIC 9(6) VALUE 0.
       01 TEMP PIC 9(6) VALUE 0.
       01 LINE-BUFFER PIC X(100) VALUE SPACES.
       01 EOF-FLAG PIC X VALUE "N".
           88 END-OF-FILE VALUE "Y".
       01 WORD PIC X(10).
       01 WORD-LENGTH PIC 9(2) VALUE 0.
       01 WORD-COUNT PIC 9(4) VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           PERFORM READ-INPUT-FILE
           PERFORM QUICK-SORT-RECURSIVE
           PERFORM WRITE-OUTPUT
           STOP RUN.

       READ-INPUT-FILE.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO INPUT-LINE
               AT END SET END-OF-FILE TO TRUE
           END-READ
           IF NOT END-OF-FILE
               PERFORM PARSE-LINE
           END-IF
           CLOSE INPUT-FILE.

       PARSE-LINE.
           MOVE 0 TO WORD-COUNT
           MOVE 1 TO I
           MOVE 1 TO J
           MOVE 0 TO N

           PERFORM UNTIL I > LENGTH OF INPUT-LINE
               IF INPUT-LINE(I:1) = SPACE OR INPUT-LINE(I:1) = X'0D'
                   IF WORD-COUNT > 0
                       MOVE WORD TO ARRAY(N+1)
                       ADD 1 TO N
                       MOVE 0 TO WORD-COUNT
                   END-IF
               ELSE
                   MOVE INPUT-LINE(I:1) TO WORD(WORD-COUNT+1:1)
                   ADD 1 TO WORD-COUNT
               END-IF
               ADD 1 TO I
           END-PERFORM.

           IF WORD-COUNT > 0
               MOVE WORD TO ARRAY(N+1)
               ADD 1 TO N
           END-IF.

       QUICK-SORT-RECURSIVE.
           PERFORM QUICK-SORT-INTERNAL WITH PARAMETERS
               01 0 TO N.

       QUICK-SORT-INTERNAL.
           01 LOW PIC 9(4) VALUE 0.
           01 HIGH PIC 9(4) VALUE 0.
           01 I PIC 9(4) VALUE 0.
           01 J PIC 9(4) VALUE 0.
           01 PIVOT PIC 9(6) VALUE 0.
           01 TEMP PIC 9(6) VALUE 0.

           IF LOW < HIGH
               PERFORM PARTITION-INTERNAL
               MOVE I TO PIVOT
               PERFORM QUICK-SORT-INTERNAL WITH PARAMETERS
                   01 LOW TO PIVOT - 1
               PERFORM QUICK-SORT-INTERNAL WITH PARAMETERS
                   01 PIVOT + 1 TO HIGH
           END-IF.

       PARTITION-INTERNAL.
           01 LOW PIC 9(4) VALUE 0.
           01 HIGH PIC 9(4) VALUE 0.
           01 I PIC 9(4) VALUE 0.
           01 J PIC 9(4) VALUE 0.
           01 PIVOT PIC 9(6) VALUE 0.
           01 TEMP PIC 9(6) VALUE 0.

           MOVE ARRAY(HIGH) TO PIVOT
           MOVE LOW - 1 TO I
           MOVE LOW TO J

           PERFORM UNTIL J >= HIGH
               IF ARRAY(J) <= PIVOT
                   ADD 1 TO I
                   MOVE ARRAY(I) TO TEMP
                   MOVE ARRAY(J) TO ARRAY(I)
                   MOVE TEMP TO ARRAY(J)
               END-IF
               ADD 1 TO J
           END-PERFORM.

           ADD 1 TO I
           MOVE ARRAY(I) TO TEMP
           MOVE ARRAY(HIGH) TO ARRAY(I)
           MOVE TEMP TO ARRAY(HIGH).

       WRITE-OUTPUT.
           OPEN OUTPUT OUTPUT-FILE
           MOVE 0 TO I
           MOVE 0 TO J
           MOVE SPACES TO LINE-BUFFER

           PERFORM VARYING I FROM 1 BY 1 UNTIL I > N
               STRING 
                   ARRAY(I) DELIMITED BY SIZE
                   " " DELIMITED BY SIZE
                   INTO LINE-BUFFER
               END-STRING
           END-PERFORM

           WRITE OUTPUT-LINE FROM LINE-BUFFER
           CLOSE OUTPUT-FILE.

       END PROGRAM QUICK-SORT.
```

## Explanation

This COBOL implementation of Quick Sort follows these key steps:

1. **Input Reading**: Reads the array size and elements from input file
2. **Array Parsing**: Parses the input line to extract individual integers
3. **Quick Sort Algorithm**: 
   - Uses recursive partitioning
   - Selects the last element as pivot
   - Partitions the array around the pivot
   - Recursively sorts sub-arrays
4. **Output Generation**: Writes the sorted array to output file

## Key Features

- **Recursive Implementation**: The quick sort uses recursion to handle sub-arrays
- **In-place Sorting**: Sorts the array without requiring additional memory
- **Pivot Selection**: Uses the last element as pivot for simplicity
- **Proper File Handling**: Reads input and writes output files correctly

## Usage

1. Save input data in `rosalind_qs.txt`
2. Compile and run the COBOL program
3. Output will be written to `output.txt`

The algorithm has O(n log n) average time complexity and O(log n) space complexity due to recursion.

